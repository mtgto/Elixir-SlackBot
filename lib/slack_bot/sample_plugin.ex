defmodule SlackBot.SamplePlugin do
  use SlackBot.Plugin
  require Logger

  def init(state) do
    Logger.debug "SlackBot.SamplePlugin.init(#{inspect state})"
    {:ok, state}
  end

  def message(message = %{"channel" => channel, "text" => text}, state) do
    Logger.debug "SlackBot.SamplePlugin.message(#{inspect message}) channel: #{channel}, text: #{text}"
    case SlackBot.strip_bot_name(text) do
      {:ok, text} ->
        SlackBot.send_message!(channel, text)
        {:reply, state}
      _ ->
        {:noreply, state}
    end
  end
end
