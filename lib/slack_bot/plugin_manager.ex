defmodule SlackBot.PluginManager do
  defmodule State do
    defstruct plugins: nil
  end

  use GenServer
  require Logger

  def start_link(plugins) do
    :ok = Logger.debug "SlackBot.PluginManager.start_link(#{inspect plugins})"
    {:ok, pid} = GenEvent.start_link(name: SlackBot.EventManager)
    :ok = Enum.each(plugins, fn([name: name, config: config]) ->
      :ok = GenEvent.add_mon_handler(pid, {SlackBot.PluginWorker, name}, [name: name, config: config])
    end)
    state = %State{plugins: plugins}
    {:ok, _pid} = GenServer.start_link(__MODULE__, state, name: SlackBot.PluginManager)
  end

  def handle_cast(msg = %{"type" => "message"}, state) do
    :ok = GenEvent.notify(SlackBot.EventManager, msg)
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

  def handle_info({:gen_event_EXIT, {SlackBot.PluginWorker, handler}, reason}, state = %State{plugins: plugins}) do
    :ok = Logger.debug "SlackBot.PluginManager.handle_info(#{inspect handler}, #{inspect reason})"
    config = Enum.find_value(plugins, fn([name: name, config: config]) ->
      if name == handler do
        config
      else
        nil
      end
    end)
    :ok = GenEvent.add_mon_handler(SlackBot.EventManager, {SlackBot.PluginWorker, handler}, [name: handler, config: config])
    {:ok, state}
  end
end
