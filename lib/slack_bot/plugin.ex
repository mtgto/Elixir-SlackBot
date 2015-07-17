defmodule SlackBot.Plugin do
  use Behaviour

  defcallback init(any) :: {:ok, any}

  @doc """
  Return plugin name.
  """
  defcallback name :: String.t

  @doc """
  Return the usage of the plugin.
  """
  defcallback usage :: String.t

  @doc """
  Call when new message has received.

  You should return whether your plugin will reply to the message or not as soon as possible.

  If you want to execute too long time (like using HTTP REST API), you should execute it in your own process.
  """
  defcallback message(Maps.t, any) :: {:reply | :noreply, any}

  defmacro __using__(_) do
    quote do
      @behaviour SlackBot.Plugin

      def init(state) do
        {:ok, state}
      end

      def name do
        "No name"
      end

      def usage do
        "No usage"
      end

      def message(_message, state) do
        {:noreply, state}
      end

      defoverridable [init: 1, name: 0, usage: 0, message: 2]
    end
  end
end
