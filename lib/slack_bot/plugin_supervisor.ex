defmodule SlackBot.PluginSupervisor do
  use Supervisor
  require Logger

  def start_link(plugins) do
    Logger.debug "SlackBot.PluginSupervisor.start_link"
    Supervisor.start_link(__MODULE__, [plugins], name: __MODULE__)
  end

  def init(plugins) do
    Logger.debug "SlackBot.PluginSupervisor.init(#{inspect plugins})"
    children = plugins |> Enum.map fn([name: name, config: config]) ->
      worker(SlackBot.PluginWorker, [{name, config}])
    end
    supervise(children, strategy: :one_for_one)
  end
end
