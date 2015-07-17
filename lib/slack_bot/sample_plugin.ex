defmodule SlackBot.SamplePlugin do
  use SlackBot.Plugin
  require Logger

  def init(state) do
    Logger.debug "SlackBot.SamplePlugin.init(#{inspect state})"
    {:ok, state}
  end

  def message(message = %{"channel" => channel, "text" => text}, state) do
    Logger.debug "SlackBot.SamplePlugin.message(#{inspect message}) channel: #{channel}, text: #{text}"
    if String.starts_with?(text, "elixir: ") do
      SlackBot.send_message!(channel, text)
      {:reply, state}
    else
      {:noreply, state}
    end
  end
end
