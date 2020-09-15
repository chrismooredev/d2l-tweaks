
type Incomplete<T> = { [Tk in keyof T]: T[Tk] } & { [untyped: string]: unknown; }

type D2L_OR_Object = OR_Objects.Func | OR_Objects.Pipe | OR_Objects.Url | OR_Objects.Call | OR_Objects.Id;
namespace OR_Objects {
	interface Func {
		_type: "func",
		D?: boolean,
		N: string,
		P: (boolean | string | D2L_OR_Object)[],
	};
	interface Pipe {
		_type: "pipe",
		P: D2L_OR_Object[],
	}
	interface Url {
		_type: "url",
		Url: string,
	}
	interface Call {
		_type: "call",
		D?: boolean,
		N: string,
		P: (string | D2L_OR_Object)[],
	}
	interface Id {
		_type: "id",
		Value: string,
	}
	interface Dictionary {
		_type: "D2L.LP.Util.Dictionary",
		action?: string,
		downloadType?: string,
		jobId?: D2L_OR_Object,
		orgUnitId?: number,
	}
}

declare var D2L: Incomplete<{
	OR: Incomplete<{
		/** Contains JSON strings of type @type {D2L_OR_Object} */
		__g1: { [n: number]: string }
	}>,
}>;
