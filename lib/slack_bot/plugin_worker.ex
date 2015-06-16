defmodule SlackBot.PluginWorker do
  defmodule State do
    defstruct module: nil, state: nil
  end

  use GenEvent
  require Logger

  @spec init([module: module, config: any]) :: {:ok, State.t}
  def init(name: module, config: config) do
    Logger.debug "SlackBot.PluginWorker.init(#{inspect module}, #{inspect config})"
    {:ok, state} = module.init(config)
    {:ok, %State{module: module, state: state}}
  end

  def handle_event(args, state = %State{module: module, state: module_state}) do
    Logger.debug "SlackBot.PluginWorker.handle_event(#{inspect args}, #{inspect state})"
    new_module_state = module.message(args, module_state)
    {:ok, %State{state | state: new_module_state}}
  end

  def handle_call(args, state = %State{module: module, state: module_state}) do
    Logger.debug "SlackBot.PluginWorker.handle_call(#{inspect args}, #{inspect state})"
    new_module_state = module.message(args, module_state)
    {:ok, :ok, %State{state | state: new_module_state}}
  end
end
