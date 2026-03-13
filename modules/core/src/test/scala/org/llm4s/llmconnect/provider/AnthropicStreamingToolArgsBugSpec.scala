package org.llm4s.llmconnect.provider

import org.llm4s.llmconnect.streaming.{ AnthropicStreamingHandler, StreamingAccumulator }
import org.llm4s.llmconnect.model.{ StreamedChunk, ToolCall }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AnthropicStreamingToolArgsBugSpec extends AnyFunSuite with Matchers {

  test("streaming accumulator assembles tool arguments from JSON fragments") {
    val acc = StreamingAccumulator.create()

    acc.addChunk(toolStartChunk("msg_1", "toolu_01A", "get_weather"))
    acc.addChunk(argFragmentChunk("msg_1", "toolu_01A", "{\"location\""))
    acc.addChunk(argFragmentChunk("msg_1", "toolu_01A", ": \"Paris\"}"))

    val toolCalls = acc.getCurrentToolCalls
    toolCalls should have size 1
    toolCalls.head.id shouldBe "toolu_01A"
    toolCalls.head.name shouldBe "get_weather"
    toolCalls.head.arguments shouldBe ujson.Obj("location" -> "Paris")
  }

  test("anthropic streaming handler routes input_json_delta by content block index") {
    val handler = new AnthropicStreamingHandler()

    handler.processChunk(
      sse(
        "message_start",
        ujson.Obj("type" -> "message_start", "message" -> ujson.Obj("id" -> "msg_1")).render()
      )
    )

    handler.processChunk(
      sse(
        "content_block_start",
        ujson
          .Obj(
            "type" -> "content_block_start",
            "index" -> 0,
            "content_block" -> ujson.Obj(
              "type" -> "tool_use",
              "id" -> "toolu_01A",
              "name" -> "get_weather",
              "input" -> ujson.Obj()
            )
          )
          .render()
      )
    )

    handler.processChunk(
      sse(
        "content_block_delta",
        ujson
          .Obj(
            "type" -> "content_block_delta",
            "index" -> 0,
            "delta" -> ujson.Obj("type" -> "input_json_delta", "partial_json" -> "{\"location\"")
          )
          .render()
      )
    )

    handler.processChunk(
      sse(
        "content_block_delta",
        ujson
          .Obj(
            "type" -> "content_block_delta",
            "index" -> 0,
            "delta" -> ujson.Obj("type" -> "input_json_delta", "partial_json" -> ":\"Paris\"}")
          )
          .render()
      )
    )

    handler.processChunk(sse("message_stop", ujson.Obj("type" -> "message_stop").render()))

    val completion = handler.getCompletion.toOption.get
    completion.message.toolCalls should have size 1
    completion.message.toolCalls.head.id shouldBe "toolu_01A"
    completion.message.toolCalls.head.name shouldBe "get_weather"
    completion.message.toolCalls.head.arguments shouldBe ujson.Obj("location" -> "Paris")
  }

  private def sse(eventType: String, data: String): String =
    s"event: $eventType\ndata: $data\n\n"

  private def toolStartChunk(messageId: String, toolCallId: String, name: String): StreamedChunk =
    StreamedChunk(
      id = messageId,
      content = None,
      toolCall = Some(ToolCall(id = toolCallId, name = name, arguments = ujson.Obj())),
      finishReason = None
    )

  private def argFragmentChunk(messageId: String, toolCallId: String, partialJson: String): StreamedChunk =
    StreamedChunk(
      id = messageId,
      content = None,
      toolCall = Some(ToolCall(id = toolCallId, name = "", arguments = ujson.Str(partialJson))),
      finishReason = None
    )
}
