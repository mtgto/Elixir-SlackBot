defmodule SlackBot.Plugin do
  use Behaviour

  defcallback init(any) :: {:ok, any}

  defcallback name :: String.t

  defcallback usage :: String.t

  defcallback message(Maps.t, any) :: any

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
        state
      end

      defoverridable [init: 1, name: 0, usage: 0, message: 2]
    end
  end
end
