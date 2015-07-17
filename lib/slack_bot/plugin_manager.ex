defmodule SlackBot.PluginManager do
  @moduledoc """
  PluginManager has a link to the supervisor which own plugin workers.
  """

  defmodule State do
    defstruct plugins: [], channels: []
  end

  use GenServer
  require Logger

  def start_link(state = %State{plugins: plugins, channels: _}) do
    :ok = Logger.debug "SlackBot.PluginManager.start_link(#{inspect plugins})"
    {:ok, _pid} = GenServer.start_link(__MODULE__, state, name: SlackBot.PluginManager)
  end

  def handle_cast(msg = %{"type" => "message", "user" => user_id, "channel" => channel}, state = %State{plugins: plugins, channels: channels}) do
    me = SlackBot.me
    cond do
      me["id"] == user_id ->
        Logger.debug "Skip the message because of mine"
      Enum.member?(channels, channel) ->
        has_reply = plugins |> Enum.any? fn([name: name, config: _config]) ->
          server_name = SlackBot.PluginWorker.name_for_module(name)
          case GenServer.call(server_name, msg) do
            {:ok, :reply} ->
              true
            {:ok, :noreply} ->
              false
          end
        end
        unless has_reply do
          Logger.debug "No plugin reply to."
        end
      true ->
        Logger.debug "Skip the message because of ignoring channels"
    end
    {:noreply, state}
  end

  def handle_cast(_msg = %{"type" => type}, state) do
    Logger.debug "SlackBot.PluginManager.handle_cast unsupported type: #{type}"
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    Logger.debug "SlackBot.PluginManager.handle_cast(#{inspect msg}, state)"
    Logger.debug "TYPE: #{msg["type"]}"
    {:noreply, state}
  end
end
