defmodule SlackBot.PluginWorker do
  @moduledoc """
  GenServer which own a plugin.
  """

  defmodule State do
    defstruct module: nil, state: nil
  end

  use GenServer
  require Logger

  def start_link({module, config}) do
    :ok = Logger.debug "SlackBot.PluginWorker.start_link"
    name = name_for_module(module)
    GenServer.start_link(__MODULE__, {module, config}, name: name)
  end

  @spec init({Atom.t, any}) :: {:ok, State.t}
  def init({module, config}) do
    Logger.debug "SlackBot.PluginWorker.init(#{inspect module}, #{inspect config})"
    {:ok, state} = module.init(config)
    {:ok, %State{module: module, state: state}}
  end

  @spec name_for_module(Atom.t) :: Atom.t
  def name_for_module(module) do
    String.to_atom("#{__MODULE__}.#{module}")
  end

  def handle_cast(args, state = %State{module: module, state: module_state}) do
    Logger.debug "SlackBot.PluginWorker.handle_cast(#{inspect args}, #{inspect state})"
    new_module_state = module.message(args, module_state)
    {:noreply, %State{state | state: new_module_state}}
  end

  def handle_call(args, _from, state = %State{module: module, state: module_state}) do
    Logger.debug "SlackBot.PluginWorker.handle_call(#{inspect args}, #{inspect state})"
    {reply, new_module_state} = module.message(args, module_state)
    {:reply, {:ok, reply}, %State{state | state: new_module_state}}
  end

  def terminate(reason, state) do
    Logger.info "SlackBot.PluginWorker.terminate(#{inspect reason}, #{inspect state})"
  end

  def handle_info(msg, state) do
    Logger.info "SlackBot.PluginWorker.handle_info(#{inspect msg}, #{inspect state})"
    {:ok, state}
  end
end
