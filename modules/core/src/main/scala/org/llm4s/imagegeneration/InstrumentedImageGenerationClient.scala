package org.llm4s.imagegeneration

import org.llm4s.metrics.{ MetricsCollector, Outcome, ErrorKind }
import org.llm4s.trace.{ Tracing, TraceEvent }

import java.nio.file.Path
import scala.annotation.unused
import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._

/**
 * Decorator that wraps an [[ImageGenerationClient]] with metrics collection
 * and trace event emission.
 *
 * Records:
 *  - `observeImageGeneration` for every generate/edit call (success or failure)
 *  - `recordImageGenerationCost` when pricing is available via [[ImagePricingRegistry]]
 *  - `TraceEvent.ImageGenerationCompleted` for observability
 *  - `TraceEvent.CostRecorded` when cost is estimated
 *
 * @param delegate   The underlying client to delegate actual generation to
 * @param config     Image generation configuration (provides model/provider info)
 * @param metrics    Metrics collector for Prometheus counters/histograms
 * @param tracing    Tracing backend for structured event emission
 */
class InstrumentedImageGenerationClient(
  delegate: ImageGenerationClient,
  config: ImageGenerationConfig,
  metrics: MetricsCollector,
  tracing: Tracing
) extends ImageGenerationClient {

  private val providerName: String = config.provider match {
    case ImageGenerationProvider.StableDiffusion => "stable-diffusion"
    case ImageGenerationProvider.DALLE           => "openai"
    case ImageGenerationProvider.HuggingFace     => "huggingface"
    case ImageGenerationProvider.StabilityAI     => "stability-ai"
  }

  override def generateImage(
    prompt: String,
    options: ImageGenerationOptions
  ): Either[ImageGenerationError, GeneratedImage] = {
    val startNanos = System.nanoTime()
    val result     = delegate.generateImage(prompt, options)
    val duration   = Duration.fromNanos(System.nanoTime() - startNanos).toMillis

    recordMetricsAndTrace("generate", result.map(Seq(_)), options, 1, duration)
    result
  }

  override def generateImages(
    prompt: String,
    count: Int,
    options: ImageGenerationOptions
  ): Either[ImageGenerationError, Seq[GeneratedImage]] = {
    val startNanos = System.nanoTime()
    val result     = delegate.generateImages(prompt, count, options)
    val duration   = Duration.fromNanos(System.nanoTime() - startNanos).toMillis

    recordMetricsAndTrace("generate", result, options, count, duration)
    result
  }

  override def editImage(
    imagePath: Path,
    prompt: String,
    maskPath: Option[Path],
    options: ImageEditOptions
  ): Either[ImageGenerationError, Seq[GeneratedImage]] = {
    val startNanos = System.nanoTime()
    val result     = delegate.editImage(imagePath, prompt, maskPath, options)
    val duration   = Duration.fromNanos(System.nanoTime() - startNanos).toMillis

    val genOptions = ImageGenerationOptions(
      size = options.size.getOrElse(ImageSize.Square512)
    )
    recordMetricsAndTrace("edit", result, genOptions, options.n, duration)
    result
  }

  override def generateImageAsync(
    prompt: String,
    options: ImageGenerationOptions
  )(implicit ec: ExecutionContext): Future[Either[ImageGenerationError, GeneratedImage]] = {
    val startNanos = System.nanoTime()
    delegate.generateImageAsync(prompt, options).map { result =>
      val duration = Duration.fromNanos(System.nanoTime() - startNanos).toMillis
      recordMetricsAndTrace("generate", result.map(Seq(_)), options, 1, duration)
      result
    }
  }

  override def generateImagesAsync(
    prompt: String,
    count: Int,
    options: ImageGenerationOptions
  )(implicit ec: ExecutionContext): Future[Either[ImageGenerationError, Seq[GeneratedImage]]] = {
    val startNanos = System.nanoTime()
    delegate.generateImagesAsync(prompt, count, options).map { result =>
      val duration = Duration.fromNanos(System.nanoTime() - startNanos).toMillis
      recordMetricsAndTrace("generate", result, options, count, duration)
      result
    }
  }

  override def editImageAsync(
    imagePath: Path,
    prompt: String,
    maskPath: Option[Path],
    options: ImageEditOptions
  )(implicit ec: ExecutionContext): Future[Either[ImageGenerationError, Seq[GeneratedImage]]] = {
    val startNanos = System.nanoTime()
    delegate.editImageAsync(imagePath, prompt, maskPath, options).map { result =>
      val duration = Duration.fromNanos(System.nanoTime() - startNanos).toMillis
      val genOptions = ImageGenerationOptions(
        size = options.size.getOrElse(ImageSize.Square512)
      )
      recordMetricsAndTrace("edit", result, genOptions, options.n, duration)
      result
    }
  }

  override def health(): Either[ImageGenerationError, ServiceStatus] =
    delegate.health()

  private def recordMetricsAndTrace(
    operation: String,
    result: Either[ImageGenerationError, Seq[GeneratedImage]],
    options: ImageGenerationOptions,
    @unused requestedCount: Int,
    durationMs: Long
  ): Unit = {
    val durationFD = FiniteDuration(durationMs, MILLISECONDS)
    val quality    = options.quality.getOrElse("standard")
    val sizeStr    = options.size.description
    val model      = config.model
    val imageCount = result.map(_.size).getOrElse(0)

    val outcome = result match {
      case Right(_) => Outcome.Success
      case Left(_)  => Outcome.Error(ErrorKind.Unknown)
    }

    metrics.observeImageGeneration(providerName, model, operation, outcome, durationFD, imageCount)

    val costUsd = result match {
      case Right(_) =>
        val cost = ImagePricingRegistry.estimateCost(model, options.quality, options.size, imageCount)
        cost.foreach { c =>
          metrics.recordImageGenerationCost(providerName, model, c, imageCount)
          metrics.recordCost(providerName, model, c)
        }
        cost
      case Left(_) => None
    }

    val event = TraceEvent.ImageGenerationCompleted(
      model = model,
      provider = providerName,
      operation = operation,
      imageCount = imageCount,
      size = sizeStr,
      quality = quality,
      durationMs = durationMs,
      costUsd = costUsd,
      success = result.isRight,
      errorMessage = result.left.toOption.map(_.message)
    )
    tracing.traceEvent(event)

    costUsd.foreach(c => tracing.traceCost(c, model, "image_generation", 0, "image_generation"))
  }
}
