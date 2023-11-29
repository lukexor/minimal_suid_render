/**
 * Returns a number whose value is limited to the given range.
 * @param {number} value The value to be clamped
 * @param {number} min The lower boundary of the output range
 * @param {number} max The upper boundary of the output range
 * @returns {number} A number in the range [min, max]
 */
function clamp(value, min = 0, max = 1) {
  return Math.min(Math.max(min, value), max);
}
/**
 * Converts a color from CSS hex format to CSS rgb format.
 * @param {string} color - Hex color, i.e. #nnn or #nnnnnn
 * @returns {string} A CSS rgb color string
 */
function hexToRgb(color) {
  color = color.substr(1);
  const re = new RegExp(`.{1,${color.length >= 6 ? 2 : 1}}`, "g");
  let colors = color.match(re);
  if (colors && colors[0].length === 1) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    colors = colors.map((n) => n + n);
  }
  return colors
    ? `rgb${colors.length === 4 ? "a" : ""}(${colors
        .map((n, index) => {
          return index < 3
            ? parseInt(n, 16)
            : Math.round((parseInt(n, 16) / 255) * 1000) / 1000;
        })
        .join(", ")})`
    : "";
}
function intToHex(int) {
  const hex = int.toString(16);
  return hex.length === 1 ? `0${hex}` : hex;
}
/**
 * Returns an object with the type and values of a color.
 *
 * Note: Does not support rgb % values.
 * @param {string} color - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla()
 * @returns {object} - A MUI color object: {type: string, values: number[]}
 */
function decomposeColor(color) {
  // Idempotent
  if (typeof color !== "string") {
    return color;
  }
  if (color.charAt(0) === "#") {
    return decomposeColor(hexToRgb(color));
  }
  const marker = color.indexOf("(");
  const type = color.substring(0, marker);
  if (["rgb", "rgba", "hsl", "hsla", "color"].indexOf(type) === -1) {
    throw new Error(
      "MUI: Unsupported `%s` color.\n" +
        "The following formats are supported: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla(), color(). " +
        color,
    );
  }
  const valuesInput = color.substring(marker + 1, color.length - 1);
  let values;
  let colorSpace;
  if (type === "color") {
    values = valuesInput.split(" ");
    colorSpace = values.shift();
    if (values.length === 4 && values[3].charAt(0) === "/") {
      values[3] = values[3].substr(1);
    }
    if (
      ["srgb", "display-p3", "a98-rgb", "prophoto-rgb", "rec-2020"].indexOf(
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        colorSpace,
      ) === -1
    ) {
      throw new Error(
        "MUI: unsupported `%s` color space.\n" +
          "The following color spaces are supported: srgb, display-p3, a98-rgb, prophoto-rgb, rec-2020. " +
          colorSpace,
      );
    }
  } else {
    values = valuesInput.split(",");
  }
  return { type, values: values.map((value) => parseFloat(value)), colorSpace };
}
/**
 * Converts a color object with type and values to a string.
 * @param {object} color - Decomposed color
 * @param {string} color.type - One of: 'rgb', 'rgba', 'hsl', 'hsla'
 * @param {array} color.values - [n,n,n] or [n,n,n,n]
 * @returns {string} A CSS color string
 */
function recomposeColor(color) {
  const { type, colorSpace } = color;
  const { values } = color;
  let newValues;
  if (type.indexOf("rgb") !== -1) {
    // Only convert the first 3 values to int (i.e. not alpha)
    newValues = values
      .map((n, i) => (i < 3 ? parseInt(n.toString(), 10) : n))
      .join(",");
  } else if (type.indexOf("hsl") !== -1) {
    newValues = values
      .map((n, i) => (i === 1 || i === 2 ? `${n}%` : n))
      .join(",");
  }
  if (type.indexOf("color") !== -1) {
    newValues = `${colorSpace} ${values.join(" ")}`;
  } else {
    newValues = `${values.join(", ")}`;
  }
  return `${type}(${newValues})`;
}
/**
 * Converts a color from CSS rgb format to CSS hex format.
 * @param {string} color - RGB color, i.e. rgb(n, n, n)
 * @returns {string} A CSS rgb color string, i.e. #nnnnnn
 */
function rgbToHex(color) {
  // Idempotent
  if (color.indexOf("#") === 0) {
    return color;
  }
  const { values } = decomposeColor(color);
  return `#${values
    .map((n, i) => intToHex(i === 3 ? Math.round(255 * n) : n))
    .join("")}`;
}
/**
 * Converts a color from hsl format to rgb format.
 * @param {string} color - HSL color values
 * @returns {string} rgb color values
 */
function hslToRgb(color) {
  const c = decomposeColor(color);
  const { values } = c;
  const h = values[0];
  const s = values[1] / 100;
  const l = values[2] / 100;
  const a = s * Math.min(l, 1 - l);
  const f = (n, k = (n + h / 30) % 12) =>
    l - a * Math.max(Math.min(k - 3, 9 - k, 1), -1);
  let type = "rgb";
  const rgb = [
    Math.round(f(0) * 255),
    Math.round(f(8) * 255),
    Math.round(f(4) * 255),
  ];
  if (c.type === "hsla") {
    type += "a";
    rgb.push(values[3]);
  }
  return recomposeColor({ type, values: rgb });
}
/**
 * The relative brightness of any point in a color space,
 * normalized to 0 for darkest black and 1 for lightest white.
 *
 * Formula: https://www.w3.org/TR/WCAG20-TECHS/G17.html#G17-tests
 * @param {string} color - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla(), color()
 * @returns {number} The relative brightness of the color in the range 0 - 1
 */
function getLuminance(color) {
  const { type, values } = decomposeColor(color);
  let rgb = type === "hsl" ? decomposeColor(hslToRgb(color)).values : values;
  rgb = rgb.map((val) => {
    if (type !== "color") {
      val /= 255; // normalized
    }
    return val <= 0.03928 ? val / 12.92 : ((val + 0.055) / 1.055) ** 2.4;
  });
  // Truncate at 3 digits
  return Number(
    (0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2]).toFixed(3),
  );
}
/**
 * Calculates the contrast ratio between two colors.
 *
 * Formula: https://www.w3.org/TR/WCAG20-TECHS/G17.html#G17-tests
 * @param {string} foreground - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla()
 * @param {string} background - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla()
 * @returns {number} A contrast ratio value in the range 0 - 21.
 */
function getContrastRatio(foreground, background) {
  const lumA = getLuminance(foreground);
  const lumB = getLuminance(background);
  return (Math.max(lumA, lumB) + 0.05) / (Math.min(lumA, lumB) + 0.05);
}
/**
 * Sets the absolute transparency of a color.
 * Any existing alpha values are overwritten.
 * @param {string} color - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla(), color()
 * @param {number} value - value to set the alpha channel to in the range 0 - 1
 * @returns {string} A CSS color string. Hex input values are returned as rgb
 */
function alpha(color, value) {
  // eslint-disable-next-line prefer-const
  let { type, values } = decomposeColor(color);
  value = clamp(value);
  if (type === "rgb" || type === "hsl") {
    type += "a";
  }
  if (type === "color") {
    values[3] = `/${value}`;
  } else {
    values[3] = value;
  }
  return recomposeColor({ type, values });
}
/**
 * Darkens a color.
 * @param {string} color - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla(), color()
 * @param {number} coefficient - multiplier in the range 0 - 1
 * @returns {string} A CSS color string. Hex input values are returned as rgb
 */
function darken(inputColor, coefficient) {
  const color = decomposeColor(inputColor);
  coefficient = clamp(coefficient);
  if (color.type.indexOf("hsl") !== -1) {
    color.values[2] *= 1 - coefficient;
  } else if (
    color.type.indexOf("rgb") !== -1 ||
    color.type.indexOf("color") !== -1
  ) {
    for (let i = 0; i < 3; i += 1) {
      color.values[i] *= 1 - coefficient;
    }
  }
  return recomposeColor(color);
}
/**
 * Lightens a color.
 * @param {string} color - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla(), color()
 * @param {number} coefficient - multiplier in the range 0 - 1
 * @returns {string} A CSS color string. Hex input values are returned as rgb
 */
function lighten(inputColor, coefficient) {
  const color = decomposeColor(inputColor);
  coefficient = clamp(coefficient);
  if (color.type.indexOf("hsl") !== -1) {
    color.values[2] += (100 - color.values[2]) * coefficient;
  } else if (color.type.indexOf("rgb") !== -1) {
    for (let i = 0; i < 3; i += 1) {
      color.values[i] += (255 - color.values[i]) * coefficient;
    }
  } else if (color.type.indexOf("color") !== -1) {
    for (let i = 0; i < 3; i += 1) {
      color.values[i] += (1 - color.values[i]) * coefficient;
    }
  }
  return recomposeColor(color);
}
/**
 * Darken or lighten a color, depending on its luminance.
 * Light colors are darkened, dark colors are lightened.
 * @param {string} color - CSS color, i.e. one of: #nnn, #nnnnnn, rgb(), rgba(), hsl(), hsla(), color()
 * @param {number} coefficient=0.15 - multiplier in the range 0 - 1
 * @returns {string} A CSS color string. Hex input values are returned as rgb
 */
function emphasize(color, coefficient = 0.15) {
  return getLuminance(color) > 0.5
    ? darken(color, coefficient)
    : lighten(color, coefficient);
}

// The breakpoint **start** at this value.
// For instance with the first breakpoint xs: [xs, sm[.
const values = {
  xs: 0,
  sm: 600,
  md: 900,
  lg: 1200,
  xl: 1536, // large screens
};
function handleBreakpoints(props, propValue, styleFromPropValue) {
  const theme = props.theme || {};
  if (Array.isArray(propValue)) {
    const themeBreakpoints = theme.breakpoints;
    return propValue.reduce((acc, item, index) => {
      acc = {
        ...acc,
        ...themeBreakpoints.up(
          themeBreakpoints.keys[index],
          styleFromPropValue(propValue[index]),
        ),
      };
      return acc;
    }, {});
  }
  if (typeof propValue === "object") {
    const themeBreakpoints = theme.breakpoints;
    const keys = Object.keys(propValue);
    return keys.reduce((acc, breakpoint) => {
      // key is breakpoint
      if (
        Object.keys(themeBreakpoints.values || values).indexOf(breakpoint) !==
        -1
      ) {
        acc = {
          ...acc,
          ...themeBreakpoints.up(
            breakpoint,
            styleFromPropValue(propValue[breakpoint], breakpoint),
          ),
        };
      } else {
        const cssKey = breakpoint;
        acc[cssKey] = propValue[cssKey];
      }
      return acc;
    }, {});
  }
  const output = styleFromPropValue(propValue);
  return output;
}
function computeBreakpointsBase(values, breakpoints) {
  const base = {};
  // fixed value
  if (typeof values !== "object") {
    return base;
  }
  const breakpointsKeys = Object.keys(breakpoints);
  if (Array.isArray(values)) {
    breakpointsKeys.forEach((breakpoint, i) => {
      if (i < values.length) {
        base[breakpoint] = true;
      }
    });
  } else {
    breakpointsKeys.forEach((breakpoint) => {
      if (values[breakpoint] != null) {
        base[breakpoint] = true;
      }
    });
  }
  return base;
}
function resolveBreakpointValues(data) {
  const values = data.values;
  const base =
    data.base || computeBreakpointsBase(values, data.breakpoints || {});
  const keys = Object.keys(base);
  if (keys.length === 0) {
    return data.values;
  }
  let previous;
  return keys.reduce((acc, breakpoint, i) => {
    if (Array.isArray(values)) {
      acc[breakpoint] = values[i] != null ? values[i] : values[previous];
      previous = i;
    } else if (typeof values === "number") {
      acc[breakpoint] = values;
    } else {
      acc[breakpoint] =
        values[breakpoint] != null ? values[breakpoint] : values[previous];
      previous = breakpoint;
    }
    return acc;
  }, {});
}

const sharedConfig = {
  context: undefined,
  registry: undefined,
};

const equalFn = (a, b) => a === b;
const $PROXY = Symbol("solid-proxy");
const $TRACK = Symbol("solid-track");
const $DEVCOMP = Symbol("solid-dev-component");
const signalOptions = {
  equals: equalFn,
};
let runEffects = runQueue;
const STALE = 1;
const PENDING = 2;
const UNOWNED = {
  owned: null,
  cleanups: null,
  context: null,
  owner: null,
};
var Owner = null;
let Transition$1 = null;
let Listener = null;
let Updates = null;
let Effects = null;
let ExecCount = 0;
function createRoot(fn, detachedOwner) {
  const listener = Listener,
    owner = Owner,
    unowned = fn.length === 0,
    current = detachedOwner === undefined ? owner : detachedOwner,
    root = unowned
      ? UNOWNED
      : {
          owned: null,
          cleanups: null,
          context: current ? current.context : null,
          owner: current,
        },
    updateFn = unowned ? fn : () => fn(() => untrack(() => cleanNode(root)));
  Owner = root;
  Listener = null;
  try {
    return runUpdates(updateFn, true);
  } finally {
    Listener = listener;
    Owner = owner;
  }
}
function createSignal(value, options) {
  options = options ? Object.assign({}, signalOptions, options) : signalOptions;
  const s = {
    value,
    observers: null,
    observerSlots: null,
    comparator: options.equals || undefined,
  };
  const setter = (value) => {
    if (typeof value === "function") {
      value = value(s.value);
    }
    return writeSignal(s, value);
  };
  return [readSignal.bind(s), setter];
}
function createComputed(fn, value, options) {
  const c = createComputation(fn, value, true, STALE);
  updateComputation(c);
}
function createRenderEffect(fn, value, options) {
  const c = createComputation(fn, value, false, STALE);
  updateComputation(c);
}
function createEffect(fn, value, options) {
  runEffects = runUserEffects;
  const c = createComputation(fn, value, false, STALE),
    s = SuspenseContext && useContext(SuspenseContext);
  if (s) c.suspense = s;
  if (!options || !options.render) c.user = true;
  Effects ? Effects.push(c) : updateComputation(c);
}
function createMemo(fn, value, options) {
  options = options ? Object.assign({}, signalOptions, options) : signalOptions;
  const c = createComputation(fn, value, true, 0);
  c.observers = null;
  c.observerSlots = null;
  c.comparator = options.equals || undefined;
  updateComputation(c);
  return readSignal.bind(c);
}
function batch(fn) {
  return runUpdates(fn, false);
}
function untrack(fn) {
  if (Listener === null) return fn();
  const listener = Listener;
  Listener = null;
  try {
    return fn();
  } finally {
    Listener = listener;
  }
}
function on(deps, fn, options) {
  const isArray = Array.isArray(deps);
  let prevInput;
  let defer = options && options.defer;
  return (prevValue) => {
    let input;
    if (isArray) {
      input = Array(deps.length);
      for (let i = 0; i < deps.length; i++) input[i] = deps[i]();
    } else input = deps();
    if (defer) {
      defer = false;
      return undefined;
    }
    const result = untrack(() => fn(input, prevInput, prevValue));
    prevInput = input;
    return result;
  };
}
function onMount(fn) {
  createEffect(() => untrack(fn));
}
function onCleanup(fn) {
  if (Owner === null);
  else if (Owner.cleanups === null) Owner.cleanups = [fn];
  else Owner.cleanups.push(fn);
  return fn;
}
function getListener() {
  return Listener;
}
function getOwner() {
  return Owner;
}
function runWithOwner(o, fn) {
  const prev = Owner;
  const prevListener = Listener;
  Owner = o;
  Listener = null;
  try {
    return runUpdates(fn, true);
  } catch (err) {
    handleError(err);
  } finally {
    Owner = prev;
    Listener = prevListener;
  }
}
function createContext(defaultValue, options) {
  const id = Symbol("context");
  return {
    id,
    Provider: createProvider(id),
    defaultValue,
  };
}
function useContext(context) {
  return Owner && Owner.context && Owner.context[context.id] !== undefined
    ? Owner.context[context.id]
    : context.defaultValue;
}
function children(fn) {
  const children = createMemo(fn);
  const memo = createMemo(() => resolveChildren$1(children()));
  memo.toArray = () => {
    const c = memo();
    return Array.isArray(c) ? c : c != null ? [c] : [];
  };
  return memo;
}
let SuspenseContext;
function readSignal() {
  if (this.sources && this.state) {
    if (this.state === STALE) updateComputation(this);
    else {
      const updates = Updates;
      Updates = null;
      runUpdates(() => lookUpstream(this), false);
      Updates = updates;
    }
  }
  if (Listener) {
    const sSlot = this.observers ? this.observers.length : 0;
    if (!Listener.sources) {
      Listener.sources = [this];
      Listener.sourceSlots = [sSlot];
    } else {
      Listener.sources.push(this);
      Listener.sourceSlots.push(sSlot);
    }
    if (!this.observers) {
      this.observers = [Listener];
      this.observerSlots = [Listener.sources.length - 1];
    } else {
      this.observers.push(Listener);
      this.observerSlots.push(Listener.sources.length - 1);
    }
  }
  return this.value;
}
function writeSignal(node, value, isComp) {
  let current = node.value;
  if (!node.comparator || !node.comparator(current, value)) {
    node.value = value;
    if (node.observers && node.observers.length) {
      runUpdates(() => {
        for (let i = 0; i < node.observers.length; i += 1) {
          const o = node.observers[i];
          const TransitionRunning = Transition$1 && Transition$1.running;
          if (TransitionRunning && Transition$1.disposed.has(o));
          if (TransitionRunning ? !o.tState : !o.state) {
            if (o.pure) Updates.push(o);
            else Effects.push(o);
            if (o.observers) markDownstream(o);
          }
          if (!TransitionRunning) o.state = STALE;
        }
        if (Updates.length > 10e5) {
          Updates = [];
          if (false);
          throw new Error();
        }
      }, false);
    }
  }
  return value;
}
function updateComputation(node) {
  if (!node.fn) return;
  cleanNode(node);
  const owner = Owner,
    listener = Listener,
    time = ExecCount;
  Listener = Owner = node;
  runComputation(node, node.value, time);
  Listener = listener;
  Owner = owner;
}
function runComputation(node, value, time) {
  let nextValue;
  try {
    nextValue = node.fn(value);
  } catch (err) {
    if (node.pure) {
      {
        node.state = STALE;
        node.owned && node.owned.forEach(cleanNode);
        node.owned = null;
      }
    }
    node.updatedAt = time + 1;
    return handleError(err);
  }
  if (!node.updatedAt || node.updatedAt <= time) {
    if (node.updatedAt != null && "observers" in node) {
      writeSignal(node, nextValue);
    } else node.value = nextValue;
    node.updatedAt = time;
  }
}
function createComputation(fn, init, pure, state = STALE, options) {
  const c = {
    fn,
    state: state,
    updatedAt: null,
    owned: null,
    sources: null,
    sourceSlots: null,
    cleanups: null,
    value: init,
    owner: Owner,
    context: Owner ? Owner.context : null,
    pure,
  };
  if (Owner === null);
  else if (Owner !== UNOWNED) {
    {
      if (!Owner.owned) Owner.owned = [c];
      else Owner.owned.push(c);
    }
  }
  return c;
}
function runTop(node) {
  if (node.state === 0) return;
  if (node.state === PENDING) return lookUpstream(node);
  if (node.suspense && untrack(node.suspense.inFallback))
    return node.suspense.effects.push(node);
  const ancestors = [node];
  while (
    (node = node.owner) &&
    (!node.updatedAt || node.updatedAt < ExecCount)
  ) {
    if (node.state) ancestors.push(node);
  }
  for (let i = ancestors.length - 1; i >= 0; i--) {
    node = ancestors[i];
    if (node.state === STALE) {
      updateComputation(node);
    } else if (node.state === PENDING) {
      const updates = Updates;
      Updates = null;
      runUpdates(() => lookUpstream(node, ancestors[0]), false);
      Updates = updates;
    }
  }
}
function runUpdates(fn, init) {
  if (Updates) return fn();
  let wait = false;
  if (!init) Updates = [];
  if (Effects) wait = true;
  else Effects = [];
  ExecCount++;
  try {
    const res = fn();
    completeUpdates(wait);
    return res;
  } catch (err) {
    if (!wait) Effects = null;
    Updates = null;
    handleError(err);
  }
}
function completeUpdates(wait) {
  if (Updates) {
    runQueue(Updates);
    Updates = null;
  }
  if (wait) return;
  const e = Effects;
  Effects = null;
  if (e.length) runUpdates(() => runEffects(e), false);
}
function runQueue(queue) {
  for (let i = 0; i < queue.length; i++) runTop(queue[i]);
}
function runUserEffects(queue) {
  let i,
    userLength = 0;
  for (i = 0; i < queue.length; i++) {
    const e = queue[i];
    if (!e.user) runTop(e);
    else queue[userLength++] = e;
  }
  for (i = 0; i < userLength; i++) runTop(queue[i]);
}
function lookUpstream(node, ignore) {
  node.state = 0;
  for (let i = 0; i < node.sources.length; i += 1) {
    const source = node.sources[i];
    if (source.sources) {
      const state = source.state;
      if (state === STALE) {
        if (
          source !== ignore &&
          (!source.updatedAt || source.updatedAt < ExecCount)
        )
          runTop(source);
      } else if (state === PENDING) lookUpstream(source, ignore);
    }
  }
}
function markDownstream(node) {
  for (let i = 0; i < node.observers.length; i += 1) {
    const o = node.observers[i];
    if (!o.state) {
      o.state = PENDING;
      if (o.pure) Updates.push(o);
      else Effects.push(o);
      o.observers && markDownstream(o);
    }
  }
}
function cleanNode(node) {
  let i;
  if (node.sources) {
    while (node.sources.length) {
      const source = node.sources.pop(),
        index = node.sourceSlots.pop(),
        obs = source.observers;
      if (obs && obs.length) {
        const n = obs.pop(),
          s = source.observerSlots.pop();
        if (index < obs.length) {
          n.sourceSlots[s] = index;
          obs[index] = n;
          source.observerSlots[index] = s;
        }
      }
    }
  }
  if (node.owned) {
    for (i = node.owned.length - 1; i >= 0; i--) cleanNode(node.owned[i]);
    node.owned = null;
  }
  if (node.cleanups) {
    for (i = node.cleanups.length - 1; i >= 0; i--) node.cleanups[i]();
    node.cleanups = null;
  }
  node.state = 0;
}
function castError(err) {
  if (err instanceof Error) return err;
  return new Error(typeof err === "string" ? err : "Unknown error", {
    cause: err,
  });
}
function handleError(err, owner = Owner) {
  const error = castError(err);
  throw error;
}
function resolveChildren$1(children) {
  if (typeof children === "function" && !children.length)
    return resolveChildren$1(children());
  if (Array.isArray(children)) {
    const results = [];
    for (let i = 0; i < children.length; i++) {
      const result = resolveChildren$1(children[i]);
      Array.isArray(result)
        ? results.push.apply(results, result)
        : results.push(result);
    }
    return results;
  }
  return children;
}
function createProvider(id, options) {
  return function provider(props) {
    let res;
    createRenderEffect(
      () =>
        (res = untrack(() => {
          Owner.context = {
            ...Owner.context,
            [id]: props.value,
          };
          return children(() => props.children);
        })),
      undefined,
    );
    return res;
  };
}

const FALLBACK = Symbol("fallback");
function dispose(d) {
  for (let i = 0; i < d.length; i++) d[i]();
}
function mapArray(list, mapFn, options = {}) {
  let items = [],
    mapped = [],
    disposers = [],
    len = 0,
    indexes = mapFn.length > 1 ? [] : null;
  onCleanup(() => dispose(disposers));
  return () => {
    let newItems = list() || [],
      i,
      j;
    newItems[$TRACK];
    return untrack(() => {
      let newLen = newItems.length,
        newIndices,
        newIndicesNext,
        temp,
        tempdisposers,
        tempIndexes,
        start,
        end,
        newEnd,
        item;
      if (newLen === 0) {
        if (len !== 0) {
          dispose(disposers);
          disposers = [];
          items = [];
          mapped = [];
          len = 0;
          indexes && (indexes = []);
        }
        if (options.fallback) {
          items = [FALLBACK];
          mapped[0] = createRoot((disposer) => {
            disposers[0] = disposer;
            return options.fallback();
          });
          len = 1;
        }
      } else if (len === 0) {
        mapped = new Array(newLen);
        for (j = 0; j < newLen; j++) {
          items[j] = newItems[j];
          mapped[j] = createRoot(mapper);
        }
        len = newLen;
      } else {
        temp = new Array(newLen);
        tempdisposers = new Array(newLen);
        indexes && (tempIndexes = new Array(newLen));
        for (
          start = 0, end = Math.min(len, newLen);
          start < end && items[start] === newItems[start];
          start++
        );
        for (
          end = len - 1, newEnd = newLen - 1;
          end >= start && newEnd >= start && items[end] === newItems[newEnd];
          end--, newEnd--
        ) {
          temp[newEnd] = mapped[end];
          tempdisposers[newEnd] = disposers[end];
          indexes && (tempIndexes[newEnd] = indexes[end]);
        }
        newIndices = new Map();
        newIndicesNext = new Array(newEnd + 1);
        for (j = newEnd; j >= start; j--) {
          item = newItems[j];
          i = newIndices.get(item);
          newIndicesNext[j] = i === undefined ? -1 : i;
          newIndices.set(item, j);
        }
        for (i = start; i <= end; i++) {
          item = items[i];
          j = newIndices.get(item);
          if (j !== undefined && j !== -1) {
            temp[j] = mapped[i];
            tempdisposers[j] = disposers[i];
            indexes && (tempIndexes[j] = indexes[i]);
            j = newIndicesNext[j];
            newIndices.set(item, j);
          } else disposers[i]();
        }
        for (j = start; j < newLen; j++) {
          if (j in temp) {
            mapped[j] = temp[j];
            disposers[j] = tempdisposers[j];
            if (indexes) {
              indexes[j] = tempIndexes[j];
              indexes[j](j);
            }
          } else mapped[j] = createRoot(mapper);
        }
        mapped = mapped.slice(0, (len = newLen));
        items = newItems.slice(0);
      }
      return mapped;
    });
    function mapper(disposer) {
      disposers[j] = disposer;
      if (indexes) {
        const [s, set] = createSignal(j);
        indexes[j] = set;
        return mapFn(newItems[j], s);
      }
      return mapFn(newItems[j]);
    }
  };
}
function createComponent(Comp, props) {
  return untrack(() => Comp(props || {}));
}
function trueFn() {
  return true;
}
const propTraps = {
  get(_, property, receiver) {
    if (property === $PROXY) return receiver;
    return _.get(property);
  },
  has(_, property) {
    if (property === $PROXY) return true;
    return _.has(property);
  },
  set: trueFn,
  deleteProperty: trueFn,
  getOwnPropertyDescriptor(_, property) {
    return {
      configurable: true,
      enumerable: true,
      get() {
        return _.get(property);
      },
      set: trueFn,
      deleteProperty: trueFn,
    };
  },
  ownKeys(_) {
    return _.keys();
  },
};
function resolveSource(s) {
  return !(s = typeof s === "function" ? s() : s) ? {} : s;
}
function resolveSources() {
  for (let i = 0, length = this.length; i < length; ++i) {
    const v = this[i]();
    if (v !== undefined) return v;
  }
}
function mergeProps(...sources) {
  let proxy = false;
  for (let i = 0; i < sources.length; i++) {
    const s = sources[i];
    proxy = proxy || (!!s && $PROXY in s);
    sources[i] = typeof s === "function" ? ((proxy = true), createMemo(s)) : s;
  }
  if (proxy) {
    return new Proxy(
      {
        get(property) {
          for (let i = sources.length - 1; i >= 0; i--) {
            const v = resolveSource(sources[i])[property];
            if (v !== undefined) return v;
          }
        },
        has(property) {
          for (let i = sources.length - 1; i >= 0; i--) {
            if (property in resolveSource(sources[i])) return true;
          }
          return false;
        },
        keys() {
          const keys = [];
          for (let i = 0; i < sources.length; i++)
            keys.push(...Object.keys(resolveSource(sources[i])));
          return [...new Set(keys)];
        },
      },
      propTraps,
    );
  }
  const target = {};
  const sourcesMap = {};
  const defined = new Set();
  for (let i = sources.length - 1; i >= 0; i--) {
    const source = sources[i];
    if (!source) continue;
    const sourceKeys = Object.getOwnPropertyNames(source);
    for (let i = 0, length = sourceKeys.length; i < length; i++) {
      const key = sourceKeys[i];
      if (key === "__proto__" || key === "constructor") continue;
      const desc = Object.getOwnPropertyDescriptor(source, key);
      if (!defined.has(key)) {
        if (desc.get) {
          defined.add(key);
          Object.defineProperty(target, key, {
            enumerable: true,
            configurable: true,
            get: resolveSources.bind(
              (sourcesMap[key] = [desc.get.bind(source)]),
            ),
          });
        } else {
          if (desc.value !== undefined) defined.add(key);
          target[key] = desc.value;
        }
      } else {
        const sources = sourcesMap[key];
        if (sources) {
          if (desc.get) {
            sources.push(desc.get.bind(source));
          } else if (desc.value !== undefined) {
            sources.push(() => desc.value);
          }
        } else if (target[key] === undefined) target[key] = desc.value;
      }
    }
  }
  return target;
}
function splitProps(props, ...keys) {
  if ($PROXY in props) {
    const blocked = new Set(keys.length > 1 ? keys.flat() : keys[0]);
    const res = keys.map((k) => {
      return new Proxy(
        {
          get(property) {
            return k.includes(property) ? props[property] : undefined;
          },
          has(property) {
            return k.includes(property) && property in props;
          },
          keys() {
            return k.filter((property) => property in props);
          },
        },
        propTraps,
      );
    });
    res.push(
      new Proxy(
        {
          get(property) {
            return blocked.has(property) ? undefined : props[property];
          },
          has(property) {
            return blocked.has(property) ? false : property in props;
          },
          keys() {
            return Object.keys(props).filter((k) => !blocked.has(k));
          },
        },
        propTraps,
      ),
    );
    return res;
  }
  const otherObject = {};
  const objects = keys.map(() => ({}));
  for (const propName of Object.getOwnPropertyNames(props)) {
    const desc = Object.getOwnPropertyDescriptor(props, propName);
    const isDefaultDesc =
      !desc.get &&
      !desc.set &&
      desc.enumerable &&
      desc.writable &&
      desc.configurable;
    let blocked = false;
    let objectIndex = 0;
    for (const k of keys) {
      if (k.includes(propName)) {
        blocked = true;
        isDefaultDesc
          ? (objects[objectIndex][propName] = desc.value)
          : Object.defineProperty(objects[objectIndex], propName, desc);
      }
      ++objectIndex;
    }
    if (!blocked) {
      isDefaultDesc
        ? (otherObject[propName] = desc.value)
        : Object.defineProperty(otherObject, propName, desc);
    }
  }
  return [...objects, otherObject];
}
let counter = 0;
function createUniqueId$1() {
  const ctx = sharedConfig.context;
  return ctx ? `${ctx.id}${ctx.count++}` : `cl-${counter++}`;
}

const narrowedError = (name) => `Stale read from <${name}>.`;
function For(props) {
  const fallback = "fallback" in props && {
    fallback: () => props.fallback,
  };
  return createMemo(
    mapArray(() => props.each, props.children, fallback || undefined),
  );
}
function Show(props) {
  const keyed = props.keyed;
  const condition = createMemo(() => props.when, undefined, {
    equals: (a, b) => (keyed ? a === b : !a === !b),
  });
  return createMemo(
    () => {
      const c = condition();
      if (c) {
        const child = props.children;
        const fn = typeof child === "function" && child.length > 0;
        return fn
          ? untrack(() =>
              child(
                keyed
                  ? c
                  : () => {
                      if (!untrack(condition)) throw narrowedError("Show");
                      return props.when;
                    },
              ),
            )
          : child;
      }
      return props.fallback;
    },
    undefined,
    undefined,
  );
}
function Switch$1(props) {
  let keyed = false;
  const equals = (a, b) =>
    a[0] === b[0] && (keyed ? a[1] === b[1] : !a[1] === !b[1]) && a[2] === b[2];
  const conditions = children(() => props.children),
    evalConditions = createMemo(
      () => {
        let conds = conditions();
        if (!Array.isArray(conds)) conds = [conds];
        for (let i = 0; i < conds.length; i++) {
          const c = conds[i].when;
          if (c) {
            keyed = !!conds[i].keyed;
            return [i, c, conds[i]];
          }
        }
        return [-1];
      },
      undefined,
      {
        equals,
      },
    );
  return createMemo(
    () => {
      const [index, when, cond] = evalConditions();
      if (index < 0) return props.fallback;
      const c = cond.children;
      const fn = typeof c === "function" && c.length > 0;
      return fn
        ? untrack(() =>
            c(
              keyed
                ? when
                : () => {
                    if (untrack(evalConditions)[0] !== index)
                      throw narrowedError("Match");
                    return cond.when;
                  },
            ),
          )
        : c;
    },
    undefined,
    undefined,
  );
}
function Match(props) {
  return props;
}

const booleans = [
  "allowfullscreen",
  "async",
  "autofocus",
  "autoplay",
  "checked",
  "controls",
  "default",
  "disabled",
  "formnovalidate",
  "hidden",
  "indeterminate",
  "ismap",
  "loop",
  "multiple",
  "muted",
  "nomodule",
  "novalidate",
  "open",
  "playsinline",
  "readonly",
  "required",
  "reversed",
  "seamless",
  "selected",
];
const Properties = /*#__PURE__*/ new Set([
  "className",
  "value",
  "readOnly",
  "formNoValidate",
  "isMap",
  "noModule",
  "playsInline",
  ...booleans,
]);
const ChildProperties = /*#__PURE__*/ new Set([
  "innerHTML",
  "textContent",
  "innerText",
  "children",
]);
const Aliases = /*#__PURE__*/ Object.assign(Object.create(null), {
  className: "class",
  htmlFor: "for",
});
const PropAliases = /*#__PURE__*/ Object.assign(Object.create(null), {
  class: "className",
  formnovalidate: {
    $: "formNoValidate",
    BUTTON: 1,
    INPUT: 1,
  },
  ismap: {
    $: "isMap",
    IMG: 1,
  },
  nomodule: {
    $: "noModule",
    SCRIPT: 1,
  },
  playsinline: {
    $: "playsInline",
    VIDEO: 1,
  },
  readonly: {
    $: "readOnly",
    INPUT: 1,
    TEXTAREA: 1,
  },
});
function getPropAlias(prop, tagName) {
  const a = PropAliases[prop];
  return typeof a === "object" ? (a[tagName] ? a["$"] : undefined) : a;
}
const DelegatedEvents = /*#__PURE__*/ new Set([
  "beforeinput",
  "click",
  "dblclick",
  "contextmenu",
  "focusin",
  "focusout",
  "input",
  "keydown",
  "keyup",
  "mousedown",
  "mousemove",
  "mouseout",
  "mouseover",
  "mouseup",
  "pointerdown",
  "pointermove",
  "pointerout",
  "pointerover",
  "pointerup",
  "touchend",
  "touchmove",
  "touchstart",
]);
const SVGElements = /*#__PURE__*/ new Set([
  "altGlyph",
  "altGlyphDef",
  "altGlyphItem",
  "animate",
  "animateColor",
  "animateMotion",
  "animateTransform",
  "circle",
  "clipPath",
  "color-profile",
  "cursor",
  "defs",
  "desc",
  "ellipse",
  "feBlend",
  "feColorMatrix",
  "feComponentTransfer",
  "feComposite",
  "feConvolveMatrix",
  "feDiffuseLighting",
  "feDisplacementMap",
  "feDistantLight",
  "feFlood",
  "feFuncA",
  "feFuncB",
  "feFuncG",
  "feFuncR",
  "feGaussianBlur",
  "feImage",
  "feMerge",
  "feMergeNode",
  "feMorphology",
  "feOffset",
  "fePointLight",
  "feSpecularLighting",
  "feSpotLight",
  "feTile",
  "feTurbulence",
  "filter",
  "font",
  "font-face",
  "font-face-format",
  "font-face-name",
  "font-face-src",
  "font-face-uri",
  "foreignObject",
  "g",
  "glyph",
  "glyphRef",
  "hkern",
  "image",
  "line",
  "linearGradient",
  "marker",
  "mask",
  "metadata",
  "missing-glyph",
  "mpath",
  "path",
  "pattern",
  "polygon",
  "polyline",
  "radialGradient",
  "rect",
  "set",
  "stop",
  "svg",
  "switch",
  "symbol",
  "text",
  "textPath",
  "tref",
  "tspan",
  "use",
  "view",
  "vkern",
]);
const SVGNamespace = {
  xlink: "http://www.w3.org/1999/xlink",
  xml: "http://www.w3.org/XML/1998/namespace",
};

function reconcileArrays(parentNode, a, b) {
  let bLength = b.length,
    aEnd = a.length,
    bEnd = bLength,
    aStart = 0,
    bStart = 0,
    after = a[aEnd - 1].nextSibling,
    map = null;
  while (aStart < aEnd || bStart < bEnd) {
    if (a[aStart] === b[bStart]) {
      aStart++;
      bStart++;
      continue;
    }
    while (a[aEnd - 1] === b[bEnd - 1]) {
      aEnd--;
      bEnd--;
    }
    if (aEnd === aStart) {
      const node =
        bEnd < bLength
          ? bStart
            ? b[bStart - 1].nextSibling
            : b[bEnd - bStart]
          : after;
      while (bStart < bEnd) parentNode.insertBefore(b[bStart++], node);
    } else if (bEnd === bStart) {
      while (aStart < aEnd) {
        if (!map || !map.has(a[aStart])) a[aStart].remove();
        aStart++;
      }
    } else if (a[aStart] === b[bEnd - 1] && b[bStart] === a[aEnd - 1]) {
      const node = a[--aEnd].nextSibling;
      parentNode.insertBefore(b[bStart++], a[aStart++].nextSibling);
      parentNode.insertBefore(b[--bEnd], node);
      a[aEnd] = b[bEnd];
    } else {
      if (!map) {
        map = new Map();
        let i = bStart;
        while (i < bEnd) map.set(b[i], i++);
      }
      const index = map.get(a[aStart]);
      if (index != null) {
        if (bStart < index && index < bEnd) {
          let i = aStart,
            sequence = 1,
            t;
          while (++i < aEnd && i < bEnd) {
            if ((t = map.get(a[i])) == null || t !== index + sequence) break;
            sequence++;
          }
          if (sequence > index - bStart) {
            const node = a[aStart];
            while (bStart < index) parentNode.insertBefore(b[bStart++], node);
          } else parentNode.replaceChild(b[bStart++], a[aStart++]);
        } else aStart++;
      } else a[aStart++].remove();
    }
  }
}

const $$EVENTS = "_$DX_DELEGATE";
function template(html, isCE, isSVG) {
  let node;
  const create = () => {
    const t = document.createElement("template");
    t.innerHTML = html;
    return isSVG ? t.content.firstChild.firstChild : t.content.firstChild;
  };
  const fn = isCE
    ? () => untrack(() => document.importNode(node || (node = create()), true))
    : () => (node || (node = create())).cloneNode(true);
  fn.cloneNode = fn;
  return fn;
}
function delegateEvents(eventNames, document = window.document) {
  const e = document[$$EVENTS] || (document[$$EVENTS] = new Set());
  for (let i = 0, l = eventNames.length; i < l; i++) {
    const name = eventNames[i];
    if (!e.has(name)) {
      e.add(name);
      document.addEventListener(name, eventHandler);
    }
  }
}
function setAttribute(node, name, value) {
  if (value == null) node.removeAttribute(name);
  else node.setAttribute(name, value);
}
function setAttributeNS(node, namespace, name, value) {
  if (value == null) node.removeAttributeNS(namespace, name);
  else node.setAttributeNS(namespace, name, value);
}
function className(node, value) {
  if (value == null) node.removeAttribute("class");
  else node.className = value;
}
function addEventListener(node, name, handler, delegate) {
  if (delegate) {
    if (Array.isArray(handler)) {
      node[`$$${name}`] = handler[0];
      node[`$$${name}Data`] = handler[1];
    } else node[`$$${name}`] = handler;
  } else if (Array.isArray(handler)) {
    const handlerFn = handler[0];
    node.addEventListener(
      name,
      (handler[0] = (e) => handlerFn.call(node, handler[1], e)),
    );
  } else node.addEventListener(name, handler);
}
function classList(node, value, prev = {}) {
  const classKeys = Object.keys(value || {}),
    prevKeys = Object.keys(prev);
  let i, len;
  for (i = 0, len = prevKeys.length; i < len; i++) {
    const key = prevKeys[i];
    if (!key || key === "undefined" || value[key]) continue;
    toggleClassKey(node, key, false);
    delete prev[key];
  }
  for (i = 0, len = classKeys.length; i < len; i++) {
    const key = classKeys[i],
      classValue = !!value[key];
    if (!key || key === "undefined" || prev[key] === classValue || !classValue)
      continue;
    toggleClassKey(node, key, true);
    prev[key] = classValue;
  }
  return prev;
}
function style(node, value, prev) {
  if (!value) return prev ? setAttribute(node, "style") : value;
  const nodeStyle = node.style;
  if (typeof value === "string") return (nodeStyle.cssText = value);
  typeof prev === "string" && (nodeStyle.cssText = prev = undefined);
  prev || (prev = {});
  value || (value = {});
  let v, s;
  for (s in prev) {
    value[s] == null && nodeStyle.removeProperty(s);
    delete prev[s];
  }
  for (s in value) {
    v = value[s];
    if (v !== prev[s]) {
      nodeStyle.setProperty(s, v);
      prev[s] = v;
    }
  }
  return prev;
}
function spread(node, props = {}, isSVG, skipChildren) {
  const prevProps = {};
  if (!skipChildren) {
    createRenderEffect(
      () =>
        (prevProps.children = insertExpression(
          node,
          props.children,
          prevProps.children,
        )),
    );
  }
  createRenderEffect(() => props.ref && props.ref(node));
  createRenderEffect(() => assign(node, props, isSVG, true, prevProps, true));
  return prevProps;
}
function use(fn, element, arg) {
  return untrack(() => fn(element, arg));
}
function insert(parent, accessor, marker, initial) {
  if (marker !== undefined && !initial) initial = [];
  if (typeof accessor !== "function")
    return insertExpression(parent, accessor, initial, marker);
  createRenderEffect(
    (current) => insertExpression(parent, accessor(), current, marker),
    initial,
  );
}
function assign(
  node,
  props,
  isSVG,
  skipChildren,
  prevProps = {},
  skipRef = false,
) {
  props || (props = {});
  for (const prop in prevProps) {
    if (!(prop in props)) {
      if (prop === "children") continue;
      prevProps[prop] = assignProp(
        node,
        prop,
        null,
        prevProps[prop],
        isSVG,
        skipRef,
      );
    }
  }
  for (const prop in props) {
    if (prop === "children") {
      if (!skipChildren) insertExpression(node, props.children);
      continue;
    }
    const value = props[prop];
    prevProps[prop] = assignProp(
      node,
      prop,
      value,
      prevProps[prop],
      isSVG,
      skipRef,
    );
  }
}
function toPropertyName(name) {
  return name.toLowerCase().replace(/-([a-z])/g, (_, w) => w.toUpperCase());
}
function toggleClassKey(node, key, value) {
  const classNames = key.trim().split(/\s+/);
  for (let i = 0, nameLen = classNames.length; i < nameLen; i++)
    node.classList.toggle(classNames[i], value);
}
function assignProp(node, prop, value, prev, isSVG, skipRef) {
  let isCE, isProp, isChildProp, propAlias, forceProp;
  if (prop === "style") return style(node, value, prev);
  if (prop === "classList") return classList(node, value, prev);
  if (value === prev) return prev;
  if (prop === "ref") {
    if (!skipRef) value(node);
  } else if (prop.slice(0, 3) === "on:") {
    const e = prop.slice(3);
    prev && node.removeEventListener(e, prev);
    value && node.addEventListener(e, value);
  } else if (prop.slice(0, 10) === "oncapture:") {
    const e = prop.slice(10);
    prev && node.removeEventListener(e, prev, true);
    value && node.addEventListener(e, value, true);
  } else if (prop.slice(0, 2) === "on") {
    const name = prop.slice(2).toLowerCase();
    const delegate = DelegatedEvents.has(name);
    if (!delegate && prev) {
      const h = Array.isArray(prev) ? prev[0] : prev;
      node.removeEventListener(name, h);
    }
    if (delegate || value) {
      addEventListener(node, name, value, delegate);
      delegate && delegateEvents([name]);
    }
  } else if (prop.slice(0, 5) === "attr:") {
    setAttribute(node, prop.slice(5), value);
  } else if (
    (forceProp = prop.slice(0, 5) === "prop:") ||
    (isChildProp = ChildProperties.has(prop)) ||
    (!isSVG &&
      ((propAlias = getPropAlias(prop, node.tagName)) ||
        (isProp = Properties.has(prop)))) ||
    (isCE = node.nodeName.includes("-"))
  ) {
    if (forceProp) {
      prop = prop.slice(5);
      isProp = true;
    }
    if (prop === "class" || prop === "className") className(node, value);
    else if (isCE && !isProp && !isChildProp)
      node[toPropertyName(prop)] = value;
    else node[propAlias || prop] = value;
  } else {
    const ns =
      isSVG && prop.indexOf(":") > -1 && SVGNamespace[prop.split(":")[0]];
    if (ns) setAttributeNS(node, ns, prop, value);
    else setAttribute(node, Aliases[prop] || prop, value);
  }
  return value;
}
function eventHandler(e) {
  const key = `$$${e.type}`;
  let node = (e.composedPath && e.composedPath()[0]) || e.target;
  if (e.target !== node) {
    Object.defineProperty(e, "target", {
      configurable: true,
      value: node,
    });
  }
  Object.defineProperty(e, "currentTarget", {
    configurable: true,
    get() {
      return node || document;
    },
  });
  while (node) {
    const handler = node[key];
    if (handler && !node.disabled) {
      const data = node[`${key}Data`];
      data !== undefined ? handler.call(node, data, e) : handler.call(node, e);
      if (e.cancelBubble) return;
    }
    node = node._$host || node.parentNode || node.host;
  }
}
function insertExpression(parent, value, current, marker, unwrapArray) {
  while (typeof current === "function") current = current();
  if (value === current) return current;
  const t = typeof value,
    multi = marker !== undefined;
  parent = (multi && current[0] && current[0].parentNode) || parent;
  if (t === "string" || t === "number") {
    if (t === "number") value = value.toString();
    if (multi) {
      let node = current[0];
      if (node && node.nodeType === 3) {
        node.data = value;
      } else node = document.createTextNode(value);
      current = cleanChildren(parent, current, marker, node);
    } else {
      if (current !== "" && typeof current === "string") {
        current = parent.firstChild.data = value;
      } else current = parent.textContent = value;
    }
  } else if (value == null || t === "boolean") {
    current = cleanChildren(parent, current, marker);
  } else if (t === "function") {
    createRenderEffect(() => {
      let v = value();
      while (typeof v === "function") v = v();
      current = insertExpression(parent, v, current, marker);
    });
    return () => current;
  } else if (Array.isArray(value)) {
    const array = [];
    const currentArray = current && Array.isArray(current);
    if (normalizeIncomingArray(array, value, current, unwrapArray)) {
      createRenderEffect(
        () =>
          (current = insertExpression(parent, array, current, marker, true)),
      );
      return () => current;
    }
    if (array.length === 0) {
      current = cleanChildren(parent, current, marker);
      if (multi) return current;
    } else if (currentArray) {
      if (current.length === 0) {
        appendNodes(parent, array, marker);
      } else reconcileArrays(parent, current, array);
    } else {
      current && cleanChildren(parent);
      appendNodes(parent, array);
    }
    current = array;
  } else if (value.nodeType) {
    if (Array.isArray(current)) {
      if (multi)
        return (current = cleanChildren(parent, current, marker, value));
      cleanChildren(parent, current, null, value);
    } else if (current == null || current === "" || !parent.firstChild) {
      parent.appendChild(value);
    } else parent.replaceChild(value, parent.firstChild);
    current = value;
  } else console.warn(`Unrecognized value. Skipped inserting`, value);
  return current;
}
function normalizeIncomingArray(normalized, array, current, unwrap) {
  let dynamic = false;
  for (let i = 0, len = array.length; i < len; i++) {
    let item = array[i],
      prev = current && current[i],
      t;
    if (item == null || item === true || item === false);
    else if ((t = typeof item) === "object" && item.nodeType) {
      normalized.push(item);
    } else if (Array.isArray(item)) {
      dynamic = normalizeIncomingArray(normalized, item, prev) || dynamic;
    } else if (t === "function") {
      if (unwrap) {
        while (typeof item === "function") item = item();
        dynamic =
          normalizeIncomingArray(
            normalized,
            Array.isArray(item) ? item : [item],
            Array.isArray(prev) ? prev : [prev],
          ) || dynamic;
      } else {
        normalized.push(item);
        dynamic = true;
      }
    } else {
      const value = String(item);
      if (prev && prev.nodeType === 3 && prev.data === value)
        normalized.push(prev);
      else normalized.push(document.createTextNode(value));
    }
  }
  return dynamic;
}
function appendNodes(parent, array, marker = null) {
  for (let i = 0, len = array.length; i < len; i++)
    parent.insertBefore(array[i], marker);
}
function cleanChildren(parent, current, marker, replacement) {
  if (marker === undefined) return (parent.textContent = "");
  const node = replacement || document.createTextNode("");
  if (current.length) {
    let inserted = false;
    for (let i = current.length - 1; i >= 0; i--) {
      const el = current[i];
      if (node !== el) {
        const isParent = el.parentNode === parent;
        if (!inserted && !i)
          isParent
            ? parent.replaceChild(node, el)
            : parent.insertBefore(node, marker);
        else isParent && el.remove();
      } else inserted = true;
    }
  } else parent.insertBefore(node, marker);
  return [node];
}
const SVG_NAMESPACE = "http://www.w3.org/2000/svg";
function createElement$1(tagName, isSVG = false) {
  return isSVG
    ? document.createElementNS(SVG_NAMESPACE, tagName)
    : document.createElement(tagName);
}
function Portal$1(props) {
  const { useShadow } = props,
    marker = document.createTextNode(""),
    mount = () => props.mount || document.body,
    owner = getOwner();
  let content;
  let hydrating = !!sharedConfig.context;
  createEffect(
    () => {
      content ||
        (content = runWithOwner(owner, () => createMemo(() => props.children)));
      const el = mount();
      if (el instanceof HTMLHeadElement) {
        const [clean, setClean] = createSignal(false);
        const cleanup = () => setClean(true);
        createRoot((dispose) =>
          insert(el, () => (!clean() ? content() : dispose()), null),
        );
        onCleanup(cleanup);
      } else {
        const container = createElement$1(
            props.isSVG ? "g" : "div",
            props.isSVG,
          ),
          renderRoot =
            useShadow && container.attachShadow
              ? container.attachShadow({
                  mode: "open",
                })
              : container;
        Object.defineProperty(container, "_$host", {
          get() {
            return marker.parentNode;
          },
          configurable: true,
        });
        insert(renderRoot, content);
        el.appendChild(container);
        props.ref && props.ref(container);
        onCleanup(() => el.removeChild(container));
      }
    },
    undefined,
    {
      render: !hydrating,
    },
  );
  return marker;
}
function Dynamic$1(props) {
  const [p, others] = splitProps(props, ["component"]);
  const cached = createMemo(() => p.component);
  return createMemo(() => {
    const component = cached();
    switch (typeof component) {
      case "function":
        Object.assign(component, {
          [$DEVCOMP]: true,
        });
        return untrack(() => component(others));
      case "string":
        const isSvg = SVGElements.has(component);
        const el = createElement$1(component, isSvg);
        spread(el, others, isSvg);
        return el;
    }
  });
}

const defaultStyledEngineContextValue = {
  cleanupStyles: true,
};
const StyledEngineContext = createContext(defaultStyledEngineContextValue);

function StyledEngineProvider(inProps) {
  const value = {
    get cache() {
      return inProps.value?.cache ?? inProps.cache;
    },
    get injectFirst() {
      return inProps.value?.injectFirst ?? inProps.injectFirst;
    },
    get cleanupStyles() {
      return (
        inProps.value?.cleanupStyles ??
        inProps.cleanupStyles ??
        defaultStyledEngineContextValue.cleanupStyles
      );
    },
  };
  return createComponent(StyledEngineContext.Provider, {
    value: value,
    get children() {
      return inProps.children;
    },
  });
}

const common = {
  black: "#000",
  white: "#fff",
};

const red = {
  50: "#ffebee",
  100: "#ffcdd2",
  200: "#ef9a9a",
  300: "#e57373",
  400: "#ef5350",
  500: "#f44336",
  600: "#e53935",
  700: "#d32f2f",
  800: "#c62828",
  900: "#b71c1c",
  A100: "#ff8a80",
  A200: "#ff5252",
  A400: "#ff1744",
  A700: "#d50000",
};

const purple = {
  50: "#f3e5f5",
  100: "#e1bee7",
  200: "#ce93d8",
  300: "#ba68c8",
  400: "#ab47bc",
  500: "#9c27b0",
  600: "#8e24aa",
  700: "#7b1fa2",
  800: "#6a1b9a",
  900: "#4a148c",
  A100: "#ea80fc",
  A200: "#e040fb",
  A400: "#d500f9",
  A700: "#aa00ff",
};

const blue = {
  50: "#e3f2fd",
  100: "#bbdefb",
  200: "#90caf9",
  300: "#64b5f6",
  400: "#42a5f5",
  500: "#2196f3",
  600: "#1e88e5",
  700: "#1976d2",
  800: "#1565c0",
  900: "#0d47a1",
  A100: "#82b1ff",
  A200: "#448aff",
  A400: "#2979ff",
  A700: "#2962ff",
};

const lightBlue = {
  50: "#e1f5fe",
  100: "#b3e5fc",
  200: "#81d4fa",
  300: "#4fc3f7",
  400: "#29b6f6",
  500: "#03a9f4",
  600: "#039be5",
  700: "#0288d1",
  800: "#0277bd",
  900: "#01579b",
  A100: "#80d8ff",
  A200: "#40c4ff",
  A400: "#00b0ff",
  A700: "#0091ea",
};

const green = {
  50: "#e8f5e9",
  100: "#c8e6c9",
  200: "#a5d6a7",
  300: "#81c784",
  400: "#66bb6a",
  500: "#4caf50",
  600: "#43a047",
  700: "#388e3c",
  800: "#2e7d32",
  900: "#1b5e20",
  A100: "#b9f6ca",
  A200: "#69f0ae",
  A400: "#00e676",
  A700: "#00c853",
};

const orange = {
  50: "#fff3e0",
  100: "#ffe0b2",
  200: "#ffcc80",
  300: "#ffb74d",
  400: "#ffa726",
  500: "#ff9800",
  600: "#fb8c00",
  700: "#f57c00",
  800: "#ef6c00",
  900: "#e65100",
  A100: "#ffd180",
  A200: "#ffab40",
  A400: "#ff9100",
  A700: "#ff6d00",
};

const grey = {
  50: "#fafafa",
  100: "#f5f5f5",
  200: "#eeeeee",
  300: "#e0e0e0",
  400: "#bdbdbd",
  500: "#9e9e9e",
  600: "#757575",
  700: "#616161",
  800: "#424242",
  900: "#212121",
  A100: "#f5f5f5",
  A200: "#eeeeee",
  A400: "#bdbdbd",
  A700: "#616161",
};

function isPlainObject(item) {
  return (
    item !== null && typeof item === "object" && item.constructor === Object
  );
}
function sortKeys(object, keys) {
  for (const key of keys) {
    const value = object[key];
    delete object[key];
    object[key] = value;
  }
}
function deepmerge(target, source, options = { clone: true }) {
  const output = options.clone ? { ...target } : target;
  const sourceKeys = options.sortKeys ? [] : undefined;
  if (isPlainObject(target) && isPlainObject(source)) {
    Object.keys(source).forEach((key) => {
      // Avoid prototype pollution
      if (key === "__proto__") {
        return;
      }
      if (sourceKeys) sourceKeys.push(key);
      let sourceValue;
      let targetValue;
      if (
        isPlainObject((sourceValue = source[key])) &&
        key in target &&
        isPlainObject((targetValue = target[key]))
      ) {
        // Since `output` is a clone of `target` and we have narrowed `target` in this block we can cast to the same type.
        output[key] = deepmerge(targetValue, sourceValue, options);
      } else if (sourceValue !== undefined) {
        output[key] = sourceValue;
      }
    });
    if (sourceKeys) sortKeys(output, sourceKeys);
  }
  return output;
}

function cloneObject(target) {
  if (Array.isArray(target)) {
    const output = [];
    for (const value of target) {
      output.push(cloneObject(value));
    }
    return output;
  } else if (isPlainObject(target)) {
    const output = {};
    for (const key in target) {
      if (key === "__proto__") {
        continue;
      }
      output[key] = cloneObject(target[key]);
    }
    return output;
  } else {
    return target;
  }
}

function merge(target, ...sources) {
  for (const source of sources)
    deepmerge(target, cloneObject(source), {
      clone: false,
    });
  return target;
}

const useLightOptions = () => ({
  text: {
    primary: "rgba(0, 0, 0, 0.87)",
    secondary: "rgba(0, 0, 0, 0.6)",
    disabled: "rgba(0, 0, 0, 0.38)",
  },
  divider: "rgba(0, 0, 0, 0.12)",
  background: {
    paper: common.white,
    default: common.white,
  },
  action: {
    active: "rgba(0, 0, 0, 0.54)",
    hover: "rgba(0, 0, 0, 0.04)",
    hoverOpacity: 0.04,
    selected: "rgba(0, 0, 0, 0.08)",
    selectedOpacity: 0.08,
    disabled: "rgba(0, 0, 0, 0.26)",
    disabledBackground: "rgba(0, 0, 0, 0.12)",
    disabledOpacity: 0.38,
    focus: "rgba(0, 0, 0, 0.12)",
    focusOpacity: 0.12,
    activatedOpacity: 0.12,
  },
});
const lightColors = {
  primary: {
    main: blue[700],
    light: blue[400],
    dark: blue[800],
  },
  secondary: {
    main: purple[500],
    light: purple[300],
    dark: purple[700],
  },
  error: {
    main: red[700],
    light: red[400],
    dark: red[800],
  },
  info: {
    main: lightBlue[700],
    light: lightBlue[800],
    dark: lightBlue[900],
  },
  success: {
    main: green[800],
    light: green[500],
    dark: green[900],
  },
  warning: {
    main: "#ED6C02",
    light: orange[500],
    dark: orange[900],
  },
};
const useDarkOptions = () => ({
  text: {
    primary: common.white,
    secondary: "rgba(255, 255, 255, 0.7)",
    disabled: "rgba(255, 255, 255, 0.5)",
  },
  divider: "rgba(255, 255, 255, 0.12)",
  background: {
    paper: "#121212",
    default: "#121212",
  },
  action: {
    active: common.white,
    hover: "rgba(255, 255, 255, 0.08)",
    hoverOpacity: 0.08,
    selected: "rgba(255, 255, 255, 0.16)",
    selectedOpacity: 0.16,
    disabled: "rgba(255, 255, 255, 0.3)",
    disabledBackground: "rgba(255, 255, 255, 0.12)",
    disabledOpacity: 0.38,
    focus: "rgba(255, 255, 255, 0.12)",
    focusOpacity: 0.12,
    activatedOpacity: 0.24,
  },
});
const darkColors = {
  primary: {
    main: blue[200],
    light: blue[50],
    dark: blue[400],
  },
  secondary: {
    main: purple[200],
    light: purple[50],
    dark: purple[400],
  },
  error: {
    main: red[500],
    light: red[300],
    dark: red[700],
  },
  info: {
    main: lightBlue[400],
    light: lightBlue[300],
    dark: lightBlue[700],
  },
  success: {
    main: green[400],
    light: green[300],
    dark: green[700],
  },
  warning: {
    main: orange[400],
    light: orange[300],
    dark: orange[700],
  },
};
const modes = {
  light: useLightOptions,
  dark: useDarkOptions,
};
function getContrastText(background, contrastThreshold) {
  return getContrastRatio(background, common.white) >= contrastThreshold
    ? common.white
    : "rgba(0, 0, 0, 0.87)";
}
function addLightOrDark(intent, direction, shade, tonalOffset) {
  const tonalOffsetLight =
    typeof tonalOffset === "number" ? tonalOffset : tonalOffset.light;
  const tonalOffsetDark =
    typeof tonalOffset === "number" ? tonalOffset * 1.5 : tonalOffset.dark;
  if (!intent[direction]) {
    // eslint-disable-next-line no-prototype-builtins
    if (intent.hasOwnProperty(shade)) {
      intent[direction] = intent[shade];
    } else if (direction === "light") {
      intent.light = lighten(intent.main, tonalOffsetLight);
    } else if (direction === "dark") {
      intent.dark = darken(intent.main, tonalOffsetDark);
    }
  }
}
function augmentColor(data) {
  const color = {
    ...data.color,
  };
  const mainShade = data.mainShade ?? 500;
  if (!data.color.main && data.color[mainShade])
    color.main = data.color[mainShade];
  addLightOrDark(color, "light", data.lightShade ?? 300, data.tonalOffset);
  addLightOrDark(color, "dark", data.darkShade ?? 700, data.tonalOffset);
  if (!color.contrastText)
    color.contrastText = getContrastText(color.main, data.contrastThreshold);
  return color;
}
const usePalleteDefaults = () =>
  createPaletteOptions({
    mode: "light",
    tonalOffset: 0.2,
    contrastThreshold: 3,
    grey,
    common,
  });
function createPaletteOptions(data) {
  return data;
}
function createPalette(options) {
  const colorNames = [
    "error",
    "info",
    "primary",
    "secondary",
    "success",
    "warning",
  ];
  const palleteDefaults = usePalleteDefaults();
  const result = {
    ...merge(
      {},
      palleteDefaults,
      modes[options?.mode ?? palleteDefaults.mode](),
      options,
    ),
    isColorName(name) {
      return colorNames.includes(name);
    },
    getColorObject(color) {
      return result[color];
    },
    getColor(color) {
      return result.mode === "light" ? result[color].light : result[color].dark;
    },
    augmentColor(data) {
      return augmentColor({
        ...data,
        tonalOffset: result.tonalOffset,
        contrastThreshold: result.contrastThreshold,
      });
    },
    getContrastText(background) {
      return getContrastText(background, result.contrastThreshold);
    },
  };
  const getDefaultColor = (color) =>
    result.mode === "light" ? lightColors[color] : darkColors[color];
  result.primary = result.augmentColor({
    color: result.primary || getDefaultColor("primary"),
  });
  result.secondary = result.augmentColor({
    color: result.secondary || getDefaultColor("secondary"),
    mainShade: "A400",
    lightShade: "A200",
    darkShade: "A700",
  });
  result.error = result.augmentColor({
    color: result.error || getDefaultColor("error"),
  });
  result.warning = result.augmentColor({
    color: result.warning || getDefaultColor("warning"),
  });
  result.info = result.augmentColor({
    color: result.info || getDefaultColor("info"),
  });
  result.success = result.augmentColor({
    color: result.success || getDefaultColor("success"),
  });
  return result;
}

const componentsDefault = createComponentsOptions({});
function createComponentsOptions(options) {
  return options;
}
function createComponents(data) {
  const result = {
    ...merge({}, componentsDefault, data ?? {}),
  };
  return result;
}

function createMixins(breakpoints, spacing = void 0, mixins = {}) {
  return {
    toolbar: {
      minHeight: 56,
      [`${breakpoints.up("xs")} and (orientation: landscape)`]: {
        minHeight: 48,
      },
      [breakpoints.up("sm")]: {
        minHeight: 64,
      },
    },
    ...mixins,
  };
}

const cache = {};
const shadowKeyUmbraOpacity = 0.2;
const shadowKeyPenumbraOpacity = 0.14;
const shadowAmbientShadowOpacity = 0.12;
function createCssShadow(...px) {
  return [
    `${px[0]}px ${px[1]}px ${px[2]}px ${px[3]}px rgba(0,0,0,${shadowKeyUmbraOpacity})`,
    `${px[4]}px ${px[5]}px ${px[6]}px ${px[7]}px rgba(0,0,0,${shadowKeyPenumbraOpacity})`,
    `${px[8]}px ${px[9]}px ${px[10]}px ${px[11]}px rgba(0,0,0,${shadowAmbientShadowOpacity})`,
  ].join(",");
}
const shadows = [
  () => "none",
  () => createCssShadow(0, 2, 1, -1, 0, 1, 1, 0, 0, 1, 3, 0),
  () => createCssShadow(0, 3, 1, -2, 0, 2, 2, 0, 0, 1, 5, 0),
  () => createCssShadow(0, 3, 3, -2, 0, 3, 4, 0, 0, 1, 8, 0),
  () => createCssShadow(0, 2, 4, -1, 0, 4, 5, 0, 0, 1, 10, 0),
  () => createCssShadow(0, 3, 5, -1, 0, 5, 8, 0, 0, 1, 14, 0),
  () => createCssShadow(0, 3, 5, -1, 0, 6, 10, 0, 0, 1, 18, 0),
  () => createCssShadow(0, 4, 5, -2, 0, 7, 10, 1, 0, 2, 16, 1),
  () => createCssShadow(0, 5, 5, -3, 0, 8, 10, 1, 0, 3, 14, 2),
  () => createCssShadow(0, 5, 6, -3, 0, 9, 12, 1, 0, 3, 16, 2),
  () => createCssShadow(0, 6, 6, -3, 0, 10, 14, 1, 0, 4, 18, 3),
  () => createCssShadow(0, 6, 7, -4, 0, 11, 15, 1, 0, 4, 20, 3),
  () => createCssShadow(0, 7, 8, -4, 0, 12, 17, 2, 0, 5, 22, 4),
  () => createCssShadow(0, 7, 8, -4, 0, 13, 19, 2, 0, 5, 24, 4),
  () => createCssShadow(0, 7, 9, -4, 0, 14, 21, 2, 0, 5, 26, 4),
  () => createCssShadow(0, 8, 9, -5, 0, 15, 22, 2, 0, 6, 28, 5),
  () => createCssShadow(0, 8, 10, -5, 0, 16, 24, 2, 0, 6, 30, 5),
  () => createCssShadow(0, 8, 11, -5, 0, 17, 26, 2, 0, 6, 32, 5),
  () => createCssShadow(0, 9, 11, -5, 0, 18, 28, 2, 0, 7, 34, 6),
  () => createCssShadow(0, 9, 12, -6, 0, 19, 29, 2, 0, 7, 36, 6),
  () => createCssShadow(0, 10, 13, -6, 0, 20, 31, 3, 0, 8, 38, 7),
  () => createCssShadow(0, 10, 13, -6, 0, 21, 33, 3, 0, 8, 40, 7),
  () => createCssShadow(0, 10, 14, -6, 0, 22, 35, 3, 0, 8, 42, 7),
  () => createCssShadow(0, 11, 14, -7, 0, 23, 36, 3, 0, 9, 44, 8),
  () => createCssShadow(0, 11, 15, -7, 0, 24, 38, 3, 0, 9, 46, 8),
];
function createShadows() {
  return new Proxy([], {
    get: (target, p) => {
      if (typeof p !== "string" || isNaN(Number(p))) return target[p];
      if (p in cache) return cache[p];
      return (cache[p] = shadows[p]());
    },
  });
}

const easing = {
  easeInOut: "cubic-bezier(0.4, 0, 0.2, 1)",
  easeOut: "cubic-bezier(0.0, 0, 0.2, 1)",
  easeIn: "cubic-bezier(0.4, 0, 1, 1)",
  sharp: "cubic-bezier(0.4, 0, 0.6, 1)",
};
const duration = {
  shortest: 150,
  shorter: 200,
  short: 250,
  standard: 300,
  complex: 375,
  enteringScreen: 225,
  leavingScreen: 195,
};
function formatMs(milliseconds) {
  return `${Math.round(milliseconds)}ms`;
}
function getAutoHeightDuration(height) {
  if (!height) {
    return 0;
  }
  const constant = height / 36;
  return Math.round((4 + 15 * constant ** 0.25 + constant / 5) * 10);
}
function createTransitions(inputTransitions) {
  const mergedEasing = {
    ...easing,
    ...inputTransitions.easing,
  };
  const mergedDuration = {
    ...duration,
    ...inputTransitions.duration,
  };
  const create = (props = ["all"], options = {}) => {
    const {
      duration: durationOption = mergedDuration.standard,
      easing: easingOption = mergedEasing.easeInOut,
      delay = 0,
      ...other
    } = options;
    return (Array.isArray(props) ? props : [props])
      .map(
        (animatedProp) =>
          `${animatedProp} ${
            typeof durationOption === "string"
              ? durationOption
              : formatMs(durationOption)
          } ${easingOption} ${
            typeof delay === "string" ? delay : formatMs(delay)
          }`,
      )
      .join(",");
  };
  return {
    getAutoHeightDuration,
    create,
    ...inputTransitions,
    easing: mergedEasing,
    duration: mergedDuration,
  };
}

const fontWeight = {
  light: 300,
  regular: 400,
  medium: 500,
  bold: 700,
};
const typographyDefaults = createTypographyOptions({
  fontFamily: '"Roboto", "Helvetica", "Arial", sans-serif',
  fontSize: 14,
  htmlFontSize: 16,
  h1: {},
  h2: {},
  h3: {},
  h4: {},
  h5: {},
  h6: {},
  subtitle1: {},
  subtitle2: {},
  body1: {},
  body2: {},
  button: {},
  caption: {},
  overline: {},
  fontWeightLight: 300,
  fontWeightRegular: 400,
  fontWeightMedium: 500,
  fontWeightBold: 700,
});
function createTypographyOptions(options) {
  return options;
}
function round$1(value) {
  return Math.round(value * 1e5) / 1e5;
}
function makeVariant(
  base,
  fontWeight,
  size,
  lineHeight,
  letterSpacing,
  casing,
) {
  return {
    fontFamily: base.fontFamily,
    fontWeight,
    fontSize: base.pxToRem(size),
    lineHeight: `${lineHeight}`,
    letterSpacing: `${round$1(letterSpacing / size)}em`,
    ...(casing
      ? {
          textTransform: "uppercase",
        }
      : {}),
  };
}
function createTypography(options = {}) {
  const base = {
    fontFamily: options.fontFamily ?? typographyDefaults.fontFamily,
    fontSize: options.fontSize ?? typographyDefaults.fontSize,
    htmlFontSize: options?.htmlFontSize ?? typographyDefaults.htmlFontSize,
    pxToRem: (size) => {
      const coef = base.fontSize / 14;
      return `${(size / base.htmlFontSize) * coef}rem`;
    },
  };
  return merge(
    base,
    {
      h1: makeVariant(base, fontWeight.light, 96, 1.167, -1.5),
      h2: makeVariant(base, fontWeight.light, 60, 1.2, -0.5),
      h3: makeVariant(base, fontWeight.regular, 48, 1.167, 0),
      h4: makeVariant(base, fontWeight.regular, 34, 1.235, 0.25),
      h5: makeVariant(base, fontWeight.regular, 24, 1.334, 0),
      h6: makeVariant(base, fontWeight.medium, 20, 1.6, 0.15),
      subtitle1: makeVariant(base, fontWeight.regular, 16, 1.75, 0.15),
      subtitle2: makeVariant(base, fontWeight.medium, 14, 1.57, 0.1),
      body1: makeVariant(base, fontWeight.regular, 16, 1.5, 0.15),
      body2: makeVariant(base, fontWeight.regular, 14, 1.43, 0.15),
      button: makeVariant(base, fontWeight.medium, 14, 1.75, 0.4, true),
      caption: makeVariant(base, fontWeight.regular, 12, 1.66, 0.4),
      overline: makeVariant(base, fontWeight.regular, 12, 2.66, 1, true),
    },
    typographyDefaults,
    options,
  );
}

const zIndexDefaults = createZIndexOptions({
  mobileStepper: 1000,
  speedDial: 1050,
  appBar: 1100,
  drawer: 1200,
  modal: 1300,
  snackbar: 1400,
  tooltip: 1500,
});
function createZIndexOptions(data) {
  return data;
}
function createZIndex(data) {
  const result = {
    ...merge({}, zIndexDefaults, data),
  };
  return result;
}

function renderMediaQuery(comparator, width, units = "px") {
  let selector;
  if (comparator === "up") {
    selector = `(min-width:${width}${units})`;
  } else if (comparator === "down") {
    selector = `(max-width:${width}${units})`;
  } else if (comparator === "between") {
    const [maxW, minW] = width;
    selector = `(max-width:${maxW}${units}) and (min-width:${minW}${units})`;
  } else {
    throw new Error(`Invalid comparator: ${comparator}`);
  }
  return `@media ${selector}`;
}

const breakpointsDefault = createBreakpointsOptions({
  columns: 12,
  keys: ["xs", "sm", "md", "lg", "xl"],
  values: {
    xs: 0,
    sm: 600,
    md: 900,
    lg: 1200,
    xl: 1536,
  },
  unit: "px",
});
function createBreakpointsOptions(options) {
  return options;
}
function createBreakpoints(options) {
  const result = {
    ...breakpointsDefault,
    ...(options ?? {}),
    up: (value, css) => {
      const key = renderMediaQuery("up", result.resolve(value));
      return css ? { [key]: css } : key;
    },
    down: (value, css) => {
      const key = renderMediaQuery("down", result.resolve(value));
      return css ? { [key]: css } : key;
    },
    between: (value, css) => {
      const key = renderMediaQuery("between", [
        result.resolve(value[0]),
        result.resolve(value[1]),
      ]);
      return css ? { [key]: css } : key;
    },
    resolve: (value) =>
      typeof value === "number" ? value : result.values[value],
  };
  return result;
}

function createSpacing(options) {
  if (typeof options === "function") return options;
  const resolved = (...values) => {
    return values
      .map((v) => (typeof v === "number" ? `${v * (options ?? 8)}px` : v))
      .join(" ");
  };
  return resolved;
}

const shapeDefaults = createShapeOptions({
  borderRadius: 4,
});
function createShapeOptions(data) {
  return data;
}
function createShape(options) {
  const result = {
    ...merge({}, shapeDefaults, options),
  };
  return result;
}

function createTheme$1(input = {}) {
  const theme = {
    direction: "ltr",
    ...input,
  };
  function def(key, defaults) {
    const inputValue = input[key];
    Object.defineProperty(theme, key, {
      configurable: true,
      enumerable: true,
      ...(typeof inputValue === "function"
        ? {
            get: inputValue,
          }
        : {
            value: defaults({
              [key]: inputValue,
            }),
          }),
    });
  }
  def("breakpoints", (input) => createBreakpoints(input.breakpoints));
  def("components", (input) => createComponents(input.components));
  def("palette", (input) => createPalette(input.palette));
  def("shape", (input) => createShape(input.shape));
  def("spacing", (input) => createSpacing(input.spacing));
  def("typography", (input) => createTypography(input.typography));
  def("shadows", () => createShadows());
  def("transitions", () => createTransitions({}));
  def("zIndex", (input) => createZIndex(input.zIndex));
  def("mixins", () => createMixins(theme.breakpoints));
  return theme;
}

function makeGetDefaultTheme(createTheme) {
  let defaultTheme;
  return function getDefaultTheme() {
    if (!defaultTheme) {
      defaultTheme = createTheme();
    }
    return defaultTheme;
  };
}

const getDefaultTheme$1 = makeGetDefaultTheme(createTheme$1);

let ThemeContext;
{
  ThemeContext = createContext({});
}
const ThemeContext$1 = ThemeContext;

function createTheme(data) {
  const result = {
    direction: "ltr",
    shadows: undefined,
    transitions: undefined,
    components: undefined,
    palette: undefined,
    typography: undefined,
    zIndex: undefined,
    mixins: undefined,
    ...data,
    breakpoints: createBreakpoints(data?.breakpoints),
    shape: createShape(data?.shape),
    spacing: createSpacing(data?.spacing),
  };
  return result;
}

const getDefaultTheme = makeGetDefaultTheme(createTheme);

function isEmptyObject(object) {
  for (const _key in object) return false;
  return true;
}

function useTheme$2(defaultTheme = getDefaultTheme, Context = ThemeContext$1) {
  const theme = useContext(Context);
  if (isEmptyObject(theme) && defaultTheme) {
    if (typeof defaultTheme === "function") return defaultTheme();
    return defaultTheme;
  }
  if (!theme) throw new Error("Theme is not defined");
  return theme;
}

function useTheme$1(defaultTheme = getDefaultTheme$1) {
  return useTheme$2(defaultTheme, ThemeContext$1);
}

function useTheme(defaultTheme) {
  return useTheme$2(defaultTheme, ThemeContext$1);
}

function useThemeProps$1(options) {
  const props = [];
  const propDefaults =
    typeof options.propDefaults === "function"
      ? options.propDefaults({
          set: (v) => v,
          inProps: options.props,
        })
      : options.propDefaults;
  const theme = useTheme();
  const themeProps = theme.components?.[options.name]?.defaultProps;
  if (propDefaults) props.push(propDefaults);
  if (themeProps) props.push(themeProps);
  if (!props.length) return options.props;
  return mergeProps(...[...props, options.props]);
}

function useThemeProps(options) {
  return useThemeProps$1(options);
}

function ThemeProvider(props) {
  return createComponent(ThemeContext$1.Provider, {
    get value() {
      return props.theme;
    },
    get children() {
      return props.children;
    },
  });
}

/* eslint-disable no-constant-condition */
/* eslint-disable no-case-declarations */
function createElement(tagName, isSVG = false) {
  return isSVG
    ? document.createElementNS("http://www.w3.org/2000/svg", tagName)
    : document.createElement(tagName);
}
function createStaticComponent(component, props) {
  switch (typeof component) {
    case "function":
      Object.assign(component, {
        [$DEVCOMP]: true,
      });
      return untrack(() => component(props));
    case "string":
      const isSvg = SVGElements.has(component);
      const el = createElement(component, isSvg);
      spread(el, props, isSvg);
      return el;
  }
}
function createDynamicComponent(component, props) {
  const cached = createMemo(component);
  return createMemo(() => createStaticComponent(cached(), props));
}
// https://github.com/solidjs/solid/blob/12c0dbbbf9f9fdf798c6682e57aee8ea763cf1ba/packages/solid/web/src/index.ts#L114
function Dynamic(props) {
  const [p, others] = splitProps(props, ["$component"]);
  return createDynamicComponent(() => p.$component, others);
}

function mergeStyleProps(values) {
  const result = values.reduce((result, value) => {
    if ("name" in value) result[`--${value.name}`] = "0";
    deepmerge(result, value, {
      clone: false,
      sortKeys: true,
    });
    return result;
  }, {});
  delete result.name;
  return result;
}

function isVar(value) {
  return value.startsWith("--");
}
function isPrivateVar(value) {
  return value.startsWith("__");
}
function isSelector(value) {
  return /[^a-z-]/i.test(value) && !isVar(value);
}
function isGlobalSelector(value) {
  return value.startsWith("@global");
}
function isMediaQuery(value) {
  return value.startsWith("@media");
}
function isKeyframes(value) {
  return value.startsWith("@keyframes");
}

function snakeCase(value) {
  return value.replace(/[A-Z]/g, (letter) => `-${letter.toLowerCase()}`);
}

function renderSelector(propKey, propValue, selectors = [], options = {}) {
  const subselectors = propKey.split(",").map((v) => {
    v = v.trim();
    return v.includes("&") ? v : `& ${v}`;
  });
  return render(
    propValue,
    (selectors.length ? selectors : [""]).flatMap((selector) =>
      subselectors.map((v) => v.replace(/&/g, selector).trim()),
    ),
    {
      ...options,
    },
  );
}
function snakeCaseWithCache(name, cache) {
  if (!cache) return snakeCase(name);
  let result = cache.get(name);
  if (result) return result;
  cache.set(name, (result = snakeCase(name)));
  return result;
}
function render(css, selectors = [], options = {}) {
  const props = [];
  const rules = [];
  for (let propKey in css) {
    const propValue = css[propKey];
    if (isPrivateVar(propKey)) {
      continue;
    } else if (isGlobalSelector(propKey)) {
      for (const selector in propValue) {
        rules.push(
          ...renderSelector(selector, propValue[selector], [], options),
        );
      }
    } else if (isMediaQuery(propKey)) {
      rules.push(
        ...render(propValue, selectors, {
          ...options,
          sublevel: true,
        }).map((v) => `${propKey} {\n${v}\n}`),
      );
    } else if (isVar(propKey)) {
      if (propValue !== undefined && propValue !== null)
        props.push(`${propKey}: ${propValue};`);
    } else if (isKeyframes(propKey)) {
      const keyframes = [];
      for (const k in propValue) {
        keyframes.push(
          ...render(propValue[k], [/^\d+$/.test(k) ? `${k}%` : k], {
            ...options,
            sublevel: true,
          }),
        );
      }
      rules.push(`${propKey} {\n${keyframes.join("\n")}\n}`);
    } else if (isSelector(propKey)) {
      rules.push(...renderSelector(propKey, propValue, selectors, options));
    } else if (options.extraProperties && propKey in options.extraProperties) {
      const extraProps = options.extraProperties[propKey](propValue);
      for (const extraPropKey in extraProps) {
        const inValue = extraProps[extraPropKey];
        const value = options.onPropertyValue
          ? options.onPropertyValue(extraPropKey, inValue)
          : inValue;
        if (value !== undefined && value !== null)
          props.push(
            `${snakeCaseWithCache(
              extraPropKey,
              options.propertyNameCache,
            )}: ${value};`,
          );
      }
    } else {
      propKey = snakeCaseWithCache(propKey, options.propertyNameCache);
      const value = options.onPropertyValue
        ? options.onPropertyValue(propKey, propValue)
        : propValue;
      if (value !== undefined && value !== null)
        props.push(`${propKey}: ${value};`);
    }
  }
  const renderProps = (level) => {
    const pad = "\t".repeat(level);
    return `${pad}${props.join(`\n${pad}`)}`;
  };
  if (selectors.length) {
    const pad = options.sublevel ? `\t` : ``;
    const selectorStr = pad + selectors.join(`,\n${pad}`);
    return [
      ...(props.length
        ? [
            `${selectorStr} {\n${renderProps(
              options.sublevel ? 2 : 1,
            )}\n${pad}}`,
          ]
        : []),
      ...rules,
    ];
  } else {
    return [...(props.length ? [renderProps(0)] : []), ...rules];
  }
}

function toArray(value) {
  return value ? (Array.isArray(value) ? value : [value]) : [];
}

function resolveFunction(value) {
  if (typeof value === "function") value = value();
  return value;
}

class StyleCache {
  ids = new Map();
  rules = new Map();
  propertyNames = new Map();
  create(name, rules, componentId) {
    let styleObject = this.rules.get(rules);
    if (styleObject) return styleObject;
    let componentIndex = this.ids.get(componentId);
    // Same component (createUniqueId) but different rules
    if (typeof componentIndex === "number") {
      // Use a new id for the new rule
      this.ids.set(componentId, ++componentIndex);
      componentId += `_${componentIndex}`;
    }
    styleObject = create(name, rules, componentId);
    this.save(styleObject, rules);
    return styleObject;
  }
  save(style, rules) {
    this.ids.set(style.id, 0);
    this.rules.set(rules, style);
  }
  delete(style) {
    this.ids.delete(style.id);
    this.rules.delete(style.rules);
  }
}
function create(name, rules, id) {
  return {
    id,
    name,
    className: `${name}-${id}`,
    rules: rules.replaceAll(`$id`, `${id}`),
  };
}
function createStyleObject(options) {
  const className = `${options.name}-$id`;
  const propsValues = toArray(resolveFunction(options.props));
  const propertyNameCache = options.cache?.propertyNames;
  const rules = propsValues
    .map((v) =>
      typeof v === "string"
        ? `.${className} {\n${v}\n}`
        : render(v, [`.${className}`], {
            extraProperties: options.extraProperties,
            propertyNameCache,
          }).join("\n"),
    )
    .join("\n");
  return (
    options.cache?.create(options.name, rules, options.componentId) ||
    create(options.name, rules, options.componentId)
  );
}

function setStyleElementText(element, text) {
  if ("styleSheet" in element) {
    element["styleSheet"].cssText = text;
  } else {
    element.innerText = "";
    element.appendChild(document.createTextNode(text));
  }
}

function setAttributes(element, attributes) {
  for (const name in attributes) {
    const value = attributes[name];
    if (value !== undefined) {
      if (value === null) {
        element.removeAttribute(name);
      } else {
        element.setAttribute(name, value);
      }
    }
  }
}
function createStyleElement(css, attributes) {
  const element = document.createElement("style");
  element.type = "text/css";
  if (attributes) setAttributes(element, attributes);
  setStyleElementText(element, css);
  return element;
}

function registerStyleElementUsage(style) {
  let uses = Number(style.getAttribute("data-uses"));
  uses++;
  style.setAttribute("data-uses", uses.toString());
}

const placeholderId = "suid-injectFirst";
function appendStyleElement(css, attributes, injectFirst) {
  if (Array.isArray(css)) css = css.join("\n");
  const id = attributes?.["id"];
  const head = document.head || document.getElementsByTagName("head")[0];
  const prevElement = id && document.getElementById(id);
  if (prevElement && prevElement instanceof HTMLStyleElement) {
    setStyleElementText(prevElement, css);
    registerStyleElementUsage(prevElement);
    return prevElement;
  } else {
    if (prevElement) prevElement.remove();
    const element = createStyleElement(css, attributes);
    registerStyleElementUsage(element);
    if (injectFirst) {
      let placeholderElement = head.querySelector(`#${placeholderId}`);
      if (!placeholderElement) {
        placeholderElement = document.createElement("style");
        placeholderElement.setAttribute("id", placeholderId);
        head.prepend(placeholderElement);
      }
      head.insertBefore(element, placeholderElement);
    } else {
      head.appendChild(element);
    }
    return element;
  }
}

function findStyleElement(input, classPrefix = "css-") {
  if (typeof input === "string") return document.getElementById(input);
  const className = [...input.classList].find((name) =>
    name.startsWith(classPrefix),
  );
  if (className)
    return document.getElementById(className.slice(classPrefix.length));
}

function unregisterStyleElementUsage(style, remove = true) {
  let uses = Number(style.getAttribute("data-uses"));
  uses--;
  if (uses <= 0) {
    if (remove) {
      style.remove();
    } else {
      style.setAttribute("data-uses", "0");
    }
  } else {
    style.setAttribute("data-uses", uses.toString());
  }
}

// It should to be noted that this function isn't equivalent to `text-transform: capitalize`.
//
// A strict capitalization should uppercase the first letter of each word in the sentence.
// We only handle the first word.
function capitalize(string) {
  if (typeof string !== "string") {
    throw new Error("MUI: `capitalize(string)` expects a string argument.");
  }
  return string.charAt(0).toUpperCase() + string.slice(1);
}

function createUniqueId(idOverride) {
  return createMemo(() => idOverride?.() ?? `mui-${createUniqueId$1()}`);
}

// Corresponds to 10 frames at 60 Hz.
// A few bytes payload overhead when lodash/debounce is ~3 kB and debounce ~300 B.
function debounce$1(func, wait = 166) {
  let timeout;
  function debounced(...args) {
    const later = () => {
      // eslint-disable-next-line prefer-spread
      func.apply(null, args);
    };
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
  }
  debounced.clear = () => {
    clearTimeout(timeout);
  };
  return debounced;
}

// A change of the browser zoom change the scrollbar size.
// Credit https://github.com/twbs/bootstrap/blob/488fd8afc535ca3a6ad4dc581f5e89217b6a36ac/js/src/util/scrollbar.js#L14-L18
function getScrollbarSize(doc) {
  // https://developer.mozilla.org/en-US/docs/Web/API/Window/innerWidth#usage_notes
  const documentWidth = doc.documentElement.clientWidth;
  return Math.abs(window.innerWidth - documentWidth);
}

function ownerDocument(node) {
  return (node && node.ownerDocument) || document;
}

function ownerWindow(node) {
  const doc = ownerDocument(node);
  return doc.defaultView || window;
}

function randomString() {
  return Math.random().toString(36).substring(2, 15).slice(0, 8);
}

let hadKeyboardEvent = true;
let hadFocusVisibleRecently = false;
let hadFocusVisibleRecentlyTimeout;
const inputTypesWhitelist = {
  text: true,
  search: true,
  url: true,
  tel: true,
  email: true,
  password: true,
  number: true,
  date: true,
  month: true,
  week: true,
  time: true,
  datetime: true,
  "datetime-local": true,
};
/**
 * Computes whether the given element should automatically trigger the
 * `focus-visible` class being added, i.e. whether it should always match
 * `:focus-visible` when focused.
 * @param {Element} node
 * @returns {boolean}
 */
function focusTriggersKeyboardModality(node) {
  const { type, tagName } = node;
  if (tagName === "INPUT" && inputTypesWhitelist[type] && !node.readOnly) {
    return true;
  }
  if (tagName === "TEXTAREA" && !node.readOnly) {
    return true;
  }
  if (node.isContentEditable) {
    return true;
  }
  return false;
}
/**
 * Keep track of our keyboard modality state with `hadKeyboardEvent`.
 * If the most recent user interaction was via the keyboard;
 * and the key press did not include a meta, alt/option, or control key;
 * then the modality is keyboard. Otherwise, the modality is not keyboard.
 * @param {KeyboardEvent} event
 */
function handleKeyDown(event) {
  if (event.metaKey || event.altKey || event.ctrlKey) {
    return;
  }
  hadKeyboardEvent = true;
}
/**
 * If at any point a user clicks with a pointing device, ensure that we change
 * the modality away from keyboard.
 * This avoids the situation where a user presses a key on an already focused
 * element, and then clicks on a different element, focusing it with a
 * pointing device, while we still think we're in keyboard modality.
 */
function handlePointerDown() {
  hadKeyboardEvent = false;
}
function handleVisibilityChange() {
  if (this.visibilityState === "hidden") {
    // If the tab becomes active again, the browser will handle calling focus
    // on the element (Safari actually calls it twice).
    // If this tab change caused a blur on an element with focus-visible,
    // re-apply the class when the user switches back to the tab.
    if (hadFocusVisibleRecently) {
      hadKeyboardEvent = true;
    }
  }
}
function prepare(doc) {
  doc.addEventListener("keydown", handleKeyDown, true);
  doc.addEventListener("mousedown", handlePointerDown, true);
  doc.addEventListener("pointerdown", handlePointerDown, true);
  doc.addEventListener("touchstart", handlePointerDown, true);
  doc.addEventListener("visibilitychange", handleVisibilityChange, true);
}
function isFocusVisible(event) {
  const { target } = event;
  try {
    return target.matches(":focus-visible");
  } catch (error) {
    // Browsers not implementing :focus-visible will throw a SyntaxError.
    // We use our own heuristic for those browsers.
    // Rethrow might be better if it's not the expected error but do we really
    // want to crash if focus-visible malfunctioned?
  }
  // No need for validFocusTarget check. The user does that by attaching it to
  // focusable events only.
  return hadKeyboardEvent || focusTriggersKeyboardModality(target);
}
function useIsFocusVisible() {
  const ref = (node) => {
    if (node != null) {
      prepare(node.ownerDocument);
    }
  };
  const [isFocusVisibleRef, setFocusVisibleRef] = createSignal(false);
  /**
   * Should be called if a blur event is fired
   */
  function handleBlurVisible() {
    // checking against potential state variable does not suffice if we focus and blur synchronously.
    // React wouldn't have time to trigger a re-render so `focusVisible` would be stale.
    // Ideally we would adjust `isFocusVisible(event)` to look at `relatedTarget` for blur events.
    // This doesn't work in IE11 due to https://github.com/facebook/react/issues/3751
    // TODO: check again if React releases their internal changes to focus event handling (https://github.com/facebook/react/pull/19186).
    if (isFocusVisibleRef()) {
      // To detect a tab/window switch, we look for a blur event followed
      // rapidly by a visibility change.
      // If we don't see a visibility change within 100ms, it's probably a
      // regular focus change.
      hadFocusVisibleRecently = true;
      window.clearTimeout(hadFocusVisibleRecentlyTimeout);
      hadFocusVisibleRecentlyTimeout = window.setTimeout(() => {
        hadFocusVisibleRecently = false;
      }, 100);
      setFocusVisibleRef(false);
      return true;
    }
    return false;
  }
  /**
   * Should be called if a blur event is fired
   */
  function handleFocusVisible(event) {
    if (isFocusVisible(event)) {
      setFocusVisibleRef(true);
      return true;
    }
    return false;
  }
  return {
    isFocusVisibleRef: {
      get current() {
        return isFocusVisibleRef();
      },
    },
    onFocus: handleFocusVisible,
    onBlur: handleBlurVisible,
    ref,
  };
}

const $RAW = Symbol("store-raw"),
  $NODE = Symbol("store-node"),
  $HAS = Symbol("store-has"),
  $SELF = Symbol("store-self");
function wrap$1(value) {
  let p = value[$PROXY];
  if (!p) {
    Object.defineProperty(value, $PROXY, {
      value: (p = new Proxy(value, proxyTraps$1)),
    });
    if (!Array.isArray(value)) {
      const keys = Object.keys(value),
        desc = Object.getOwnPropertyDescriptors(value);
      for (let i = 0, l = keys.length; i < l; i++) {
        const prop = keys[i];
        if (desc[prop].get) {
          Object.defineProperty(value, prop, {
            enumerable: desc[prop].enumerable,
            get: desc[prop].get.bind(p),
          });
        }
      }
    }
  }
  return p;
}
function isWrappable(obj) {
  let proto;
  return (
    obj != null &&
    typeof obj === "object" &&
    (obj[$PROXY] ||
      !(proto = Object.getPrototypeOf(obj)) ||
      proto === Object.prototype ||
      Array.isArray(obj))
  );
}
function unwrap(item, set = new Set()) {
  let result, unwrapped, v, prop;
  if ((result = item != null && item[$RAW])) return result;
  if (!isWrappable(item) || set.has(item)) return item;
  if (Array.isArray(item)) {
    if (Object.isFrozen(item)) item = item.slice(0);
    else set.add(item);
    for (let i = 0, l = item.length; i < l; i++) {
      v = item[i];
      if ((unwrapped = unwrap(v, set)) !== v) item[i] = unwrapped;
    }
  } else {
    if (Object.isFrozen(item)) item = Object.assign({}, item);
    else set.add(item);
    const keys = Object.keys(item),
      desc = Object.getOwnPropertyDescriptors(item);
    for (let i = 0, l = keys.length; i < l; i++) {
      prop = keys[i];
      if (desc[prop].get) continue;
      v = item[prop];
      if ((unwrapped = unwrap(v, set)) !== v) item[prop] = unwrapped;
    }
  }
  return item;
}
function getNodes(target, symbol) {
  let nodes = target[symbol];
  if (!nodes)
    Object.defineProperty(target, symbol, {
      value: (nodes = Object.create(null)),
    });
  return nodes;
}
function getNode(nodes, property, value) {
  if (nodes[property]) return nodes[property];
  const [s, set] = createSignal(value, {
    equals: false,
    internal: true,
  });
  s.$ = set;
  return (nodes[property] = s);
}
function proxyDescriptor$1(target, property) {
  const desc = Reflect.getOwnPropertyDescriptor(target, property);
  if (
    !desc ||
    desc.get ||
    !desc.configurable ||
    property === $PROXY ||
    property === $NODE
  )
    return desc;
  delete desc.value;
  delete desc.writable;
  desc.get = () => target[$PROXY][property];
  return desc;
}
function trackSelf(target) {
  getListener() && getNode(getNodes(target, $NODE), $SELF)();
}
function ownKeys(target) {
  trackSelf(target);
  return Reflect.ownKeys(target);
}
const proxyTraps$1 = {
  get(target, property, receiver) {
    if (property === $RAW) return target;
    if (property === $PROXY) return receiver;
    if (property === $TRACK) {
      trackSelf(target);
      return receiver;
    }
    const nodes = getNodes(target, $NODE);
    const tracked = nodes[property];
    let value = tracked ? tracked() : target[property];
    if (property === $NODE || property === $HAS || property === "__proto__")
      return value;
    if (!tracked) {
      const desc = Object.getOwnPropertyDescriptor(target, property);
      if (
        getListener() &&
        (typeof value !== "function" || target.hasOwnProperty(property)) &&
        !(desc && desc.get)
      )
        value = getNode(nodes, property, value)();
    }
    return isWrappable(value) ? wrap$1(value) : value;
  },
  has(target, property) {
    if (
      property === $RAW ||
      property === $PROXY ||
      property === $TRACK ||
      property === $NODE ||
      property === $HAS ||
      property === "__proto__"
    )
      return true;
    getListener() && getNode(getNodes(target, $HAS), property)();
    return property in target;
  },
  set() {
    return true;
  },
  deleteProperty() {
    return true;
  },
  ownKeys: ownKeys,
  getOwnPropertyDescriptor: proxyDescriptor$1,
};
function setProperty(state, property, value, deleting = false) {
  if (!deleting && state[property] === value) return;
  const prev = state[property],
    len = state.length;
  if (value === undefined) {
    delete state[property];
    if (state[$HAS] && state[$HAS][property] && prev !== undefined)
      state[$HAS][property].$();
  } else {
    state[property] = value;
    if (state[$HAS] && state[$HAS][property] && prev === undefined)
      state[$HAS][property].$();
  }
  let nodes = getNodes(state, $NODE),
    node;
  if ((node = getNode(nodes, property, prev))) node.$(() => value);
  if (Array.isArray(state) && state.length !== len) {
    for (let i = state.length; i < len; i++) (node = nodes[i]) && node.$();
    (node = getNode(nodes, "length", len)) && node.$(state.length);
  }
  (node = nodes[$SELF]) && node.$();
}
function mergeStoreNode(state, value) {
  const keys = Object.keys(value);
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    setProperty(state, key, value[key]);
  }
}
function updateArray(current, next) {
  if (typeof next === "function") next = next(current);
  next = unwrap(next);
  if (Array.isArray(next)) {
    if (current === next) return;
    let i = 0,
      len = next.length;
    for (; i < len; i++) {
      const value = next[i];
      if (current[i] !== value) setProperty(current, i, value);
    }
    setProperty(current, "length", len);
  } else mergeStoreNode(current, next);
}
function updatePath(current, path, traversed = []) {
  let part,
    prev = current;
  if (path.length > 1) {
    part = path.shift();
    const partType = typeof part,
      isArray = Array.isArray(current);
    if (Array.isArray(part)) {
      for (let i = 0; i < part.length; i++) {
        updatePath(current, [part[i]].concat(path), traversed);
      }
      return;
    } else if (isArray && partType === "function") {
      for (let i = 0; i < current.length; i++) {
        if (part(current[i], i))
          updatePath(current, [i].concat(path), traversed);
      }
      return;
    } else if (isArray && partType === "object") {
      const { from = 0, to = current.length - 1, by = 1 } = part;
      for (let i = from; i <= to; i += by) {
        updatePath(current, [i].concat(path), traversed);
      }
      return;
    } else if (path.length > 1) {
      updatePath(current[part], path, [part].concat(traversed));
      return;
    }
    prev = current[part];
    traversed = [part].concat(traversed);
  }
  let value = path[0];
  if (typeof value === "function") {
    value = value(prev, traversed);
    if (value === prev) return;
  }
  if (part === undefined && value == undefined) return;
  value = unwrap(value);
  if (
    part === undefined ||
    (isWrappable(prev) && isWrappable(value) && !Array.isArray(value))
  ) {
    mergeStoreNode(prev, value);
  } else setProperty(current, part, value);
}
function createStore(...[store, options]) {
  const unwrappedStore = unwrap(store || {});
  const isArray = Array.isArray(unwrappedStore);
  const wrappedStore = wrap$1(unwrappedStore);
  function setStore(...args) {
    batch(() => {
      isArray && args.length === 1
        ? updateArray(unwrappedStore, args[0])
        : updatePath(unwrappedStore, args);
    });
  }
  return [wrappedStore, setStore];
}

function proxyDescriptor(target, property) {
  const desc = Reflect.getOwnPropertyDescriptor(target, property);
  if (
    !desc ||
    desc.get ||
    desc.set ||
    !desc.configurable ||
    property === $PROXY ||
    property === $NODE
  )
    return desc;
  delete desc.value;
  delete desc.writable;
  desc.get = () => target[$PROXY][property];
  desc.set = (v) => (target[$PROXY][property] = v);
  return desc;
}
const proxyTraps = {
  get(target, property, receiver) {
    if (property === $RAW) return target;
    if (property === $PROXY) return receiver;
    if (property === $TRACK) {
      trackSelf(target);
      return receiver;
    }
    const nodes = getNodes(target, $NODE);
    const tracked = nodes[property];
    let value = tracked ? tracked() : target[property];
    if (property === $NODE || property === $HAS || property === "__proto__")
      return value;
    if (!tracked) {
      const desc = Object.getOwnPropertyDescriptor(target, property);
      const isFunction = typeof value === "function";
      if (
        getListener() &&
        (!isFunction || target.hasOwnProperty(property)) &&
        !(desc && desc.get)
      )
        value = getNode(nodes, property, value)();
      else if (
        value != null &&
        isFunction &&
        value === Array.prototype[property]
      ) {
        return (...args) =>
          batch(() => Array.prototype[property].apply(receiver, args));
      }
    }
    return isWrappable(value) ? wrap(value) : value;
  },
  has(target, property) {
    if (
      property === $RAW ||
      property === $PROXY ||
      property === $TRACK ||
      property === $NODE ||
      property === $HAS ||
      property === "__proto__"
    )
      return true;
    getListener() && getNode(getNodes(target, $HAS), property)();
    return property in target;
  },
  set(target, property, value) {
    batch(() => setProperty(target, property, unwrap(value)));
    return true;
  },
  deleteProperty(target, property) {
    batch(() => setProperty(target, property, undefined, true));
    return true;
  },
  ownKeys: ownKeys,
  getOwnPropertyDescriptor: proxyDescriptor,
};
function wrap(value) {
  let p = value[$PROXY];
  if (!p) {
    Object.defineProperty(value, $PROXY, {
      value: (p = new Proxy(value, proxyTraps)),
    });
    const keys = Object.keys(value),
      desc = Object.getOwnPropertyDescriptors(value);
    for (let i = 0, l = keys.length; i < l; i++) {
      const prop = keys[i];
      if (desc[prop].get) {
        const get = desc[prop].get.bind(p);
        Object.defineProperty(value, prop, {
          get,
        });
      }
      if (desc[prop].set) {
        const og = desc[prop].set,
          set = (v) => batch(() => og.call(p, v));
        Object.defineProperty(value, prop, {
          set,
        });
      }
    }
  }
  return p;
}
function createMutable(state, options) {
  const unwrappedStore = unwrap(state || {});
  const wrappedStore = wrap(unwrappedStore);
  return wrappedStore;
}

function usePreviousProps(props, propNames) {
  const initialValue = propNames.reduce((result, name) => {
    result[name] = props[name];
    return result;
  }, {});
  const prevProps = createMutable(initialValue);
  for (const propName of propNames)
    createEffect((prev) => {
      prevProps[propName] = prev;
      return props[propName];
    }, props[propName]);
  return prevProps;
}

function parseSSRElementAttr(html, attr) {
  const tagEnd = html.indexOf(">");
  if (tagEnd === -1) throw new Error("Invalid SSR element");
  const tag = { end: tagEnd };
  const element = html.slice(0, tagEnd);
  const startAttrPattern = ` ${attr}="`;
  const startAttrIndex = element.indexOf(startAttrPattern);
  if (startAttrIndex !== -1) {
    const start = startAttrIndex + startAttrPattern.length;
    const length = element.slice(start).indexOf('"');
    if (length !== -1)
      return {
        tag,
        attr: {
          start,
          end: start + length,
        },
      };
  }
  return { tag };
}
function getSSRElementAttr(html, attr) {
  const index = parseSSRElementAttr(html, attr);
  if (index.attr) {
    return html.slice(index.attr.start, index.attr.end);
  }
}
function setSSRElementAttr(element, name, value) {
  const html = element.t;
  const index = parseSSRElementAttr(html, name);
  const resolveValue = (input) =>
    typeof value === "function" ? value(input) : value;
  if (index.attr) {
    element.t =
      html.slice(0, index.attr.start) +
      resolveValue(html.slice(index.attr.start, index.attr.end)) +
      html.slice(index.attr.end);
  } else {
    element.t =
      html.slice(0, index.tag.end) +
      ` ${name}="${resolveValue()}"` +
      html.slice(index.tag.end);
  }
}

function isSSRElement(input) {
  return (
    !!input &&
    typeof input === "object" &&
    "t" in input &&
    typeof input.t === "string" &&
    /^<[a-z]/i.test(input.t.trimStart())
  );
}
function isBrowserElement(input) {
  return globalThis.Element && input instanceof Element;
}
function isElement$1(input) {
  return isBrowserElement(input) || isSSRElement(input);
}
function getElementAttr(input, name) {
  if (isBrowserElement(input)) {
    return input.getAttribute(name) ?? undefined;
  } else if (isSSRElement(input)) {
    return getSSRElementAttr(input.t, name);
  }
}
function hasElementClass(input, name) {
  return getElementAttr(input, "class")?.split(" ").includes(name) || false;
}
function addElementClass(input, value) {
  if (isBrowserElement(input)) {
    input.classList.add(...value.split(" "));
  } else {
    setSSRElementAttr(input, "class", (prev) =>
      prev
        ? prev.split(" ").includes(value)
          ? value
          : `${prev} ${value}`
        : value,
    );
  }
}
function isSuidElement(input, component) {
  return (
    typeof component.__suid === "string" &&
    hasElementClass(input, component.toString().slice(1))
  );
}

const styleCache = new StyleCache();
function normalizeStyleProps(props) {
  if (!props) {
    return [];
  } else if (Array.isArray(props)) {
    return props.flat(Infinity).filter((v) => !!v);
  } else {
    return [props];
  }
}
function createStyleId() {
  return randomString();
}
function createStyle(value) {
  const context = useContext(StyledEngineContext);
  const [name, setName] = createSignal("");
  const componentId = createStyleId();
  let styleElement;
  let isGlobalStyleObject = false;
  createRenderEffect((prevResult) => {
    const propsValue = value();
    let styleObject;
    if (propsValue) {
      const props = mergeStyleProps(normalizeStyleProps(propsValue));
      isGlobalStyleObject = "@global" in props;
      styleObject = createStyleObject({
        name: "css",
        props,
        cache: styleCache,
        componentId,
      });
      {
        styleElement = findStyleElement(styleObject.id);
        if (styleElement) {
          registerStyleElementUsage(styleElement);
        } else {
          styleElement = appendStyleElement(
            styleObject.rules,
            {
              id: styleObject.id,
              nonce: context.cache?.nonce,
            },
            context.injectFirst,
          );
        }
        if (prevResult?.styleElement) {
          unregisterStyleElementUsage(prevResult.styleElement);
        }
      }
    }
    if (typeof styleObject?.className === "string") {
      setName(styleObject.className);
    } else {
      setName("");
    }
    return {
      className: styleObject?.className,
      styleElement,
    };
  }, undefined);
  onCleanup(() => {
    if (styleElement)
      unregisterStyleElementUsage(
        styleElement,
        context.cleanupStyles ?? isGlobalStyleObject,
      );
  });
  return name;
}

function resolve(css, onProp, cssTarget = {}) {
  for (const name in css) {
    const value = css[name];
    if (isGlobalSelector(name)) {
      cssTarget[name] = resolve(value, onProp);
    } else if (isMediaQuery(name)) {
      cssTarget[name] = resolve(value, onProp);
    } else if (isKeyframes(name)) {
      cssTarget[name] = {};
      for (const v in value) {
        cssTarget[name][v] = resolve(value[v], onProp);
      }
    } else if (isSelector(name)) {
      cssTarget[name] = resolve(value, onProp);
    } else {
      const result = onProp(name, value);
      if (result) {
        for (const key in result) {
          cssTarget[key] = isMediaQuery(key)
            ? { ...cssTarget[key], ...result[key] }
            : result[key];
        }
      } else {
        cssTarget[name] = value;
      }
    }
  }
  return cssTarget;
}

/**
 * @link https://github.com/emotion-js/emotion/blob/main/packages/unitless/src/index.js
 */
const unitLess = new Set([
  "animationIterationCount",
  "borderImageOutset",
  "borderImageSlice",
  "borderImageWidth",
  "boxFlex",
  "boxFlexGroup",
  "boxOrdinalGroup",
  "columnCount",
  "columns",
  "flex",
  "flexGrow",
  "flexPositive",
  "flexShrink",
  "flexNegative",
  "flexOrder",
  "gridRow",
  "gridRowEnd",
  "gridRowSpan",
  "gridRowStart",
  "gridColumn",
  "gridColumnEnd",
  "gridColumnSpan",
  "gridColumnStart",
  "msGridRow",
  "msGridRowSpan",
  "msGridColumn",
  "msGridColumnSpan",
  "fontWeight",
  "lineHeight",
  "opacity",
  "order",
  "orphans",
  "tabSize",
  "widows",
  "zIndex",
  "zoom",
  "WebkitLineClamp",
  "fillOpacity",
  "floodOpacity",
  "stopOpacity",
  "strokeDasharray",
  "strokeDashoffset",
  "strokeMiterlimit",
  "strokeOpacity",
  "strokeWidth",
]);
function resolveStyledPropsValue(name, value) {
  if (typeof value === "number") {
    return { [name]: unitLess.has(name) ? value.toString() : `${value}px` };
  }
}
function resolveStyledProps(v, cssTarget = {}) {
  return resolve(v, resolveStyledPropsValue, cssTarget);
}

function getThemeValue(theme, key, value) {
  if (typeof value !== "string") return value;
  const names = value.split(".");
  let ref = theme[key];
  for (let i = 0; i < names.length; i++) {
    ref = ref?.[names[i]];
    if (!ref) break;
  }
  return ref ?? value;
}

function getPath(obj, path) {
  if (!path || typeof path !== "string") {
    return null;
  }
  return path
    .split(".")
    .reduce((acc, item) => (acc && acc[item] ? acc[item] : null), obj);
}
function createUnaryUnit(theme, themeKey, defaultValue, propName) {
  const themeSpacing = getPath(theme, themeKey) || defaultValue;
  if (typeof themeSpacing === "number") {
    return (abs) => {
      if (typeof abs === "string") {
        return abs;
      }
      return themeSpacing * abs;
    };
  }
  if (Array.isArray(themeSpacing)) {
    return (abs) => {
      if (typeof abs === "string") {
        return abs;
      }
      return themeSpacing[abs];
    };
  }
  if (typeof themeSpacing === "function") {
    return themeSpacing;
  }
  return () => undefined;
}
function createUnarySpacing(theme) {
  return createUnaryUnit(theme, "spacing.resolve", 8);
}

const dirMap = {
  x: ["Left", "Right"],
  y: ["Top", "Bottom"],
};
function asPx(value) {
  return typeof value === "number" ? `${value}px` : value;
}
function customProp(name, onValue) {
  return (value, theme) => onValue(name, value, theme);
}
function prop(name, onValue) {
  return onValue
    ? (value, theme) => ({
        [name]: onValue(name, value, theme),
      })
    : (value) => ({ [name]: value });
}
function pxProp(name) {
  return prop(name, (name, value) => asPx(value));
}
function mProp(name, suffix, onValue) {
  const names = suffix.map((v) => `${name}${v}`);
  return onValue
    ? (value, theme) =>
        names.reduce((result, name) => {
          result[name] = onValue(name, value, theme);
          return result;
        }, {})
    : (value) =>
        names.reduce((result, name) => {
          result[name] = value;
          return result;
        }, {});
}
function createSystemProps() {
  return {
    ...createSystemDisplayProps(),
    ...createSystemFlexboxProps(),
    ...createSystemGridProps(),
    ...createSystemPositionProps(),
    ...createSystemPaletteProps(),
    ...createSystemSizingProps(),
    ...createSystemBorderProps(),
    ...createSystemSpacingProps(),
    ...createSystemTypographyProps(),
  };
}
function createSystemDisplayProps() {
  return {
    displayPrint: customProp("displayPrint", (name, display) => ({
      "@media print": {
        display,
      },
    })),
    displayRaw: prop("display"),
    overflow: prop("overflow"),
    textOverflow: prop("textOverflow"),
    visibility: prop("visibility"),
    whiteSpace: prop("whiteSpace"),
  };
}
function createSystemFlexboxProps() {
  return {
    flexBasis: prop("flexBasis"),
    flexDirection: prop("flexDirection"),
    flexWrap: prop("flexWrap"),
    justifyContent: prop("justifyContent"),
    alignItems: prop("alignItems"),
    alignContent: prop("alignContent"),
    order: prop("order"),
    flex: prop("flex"),
    flexGrow: prop("flexGrow"),
    flexShrink: prop("flexShrink"),
    alignSelf: prop("alignSelf"),
    justifyItems: prop("justifyItems"),
    justifySelf: prop("justifySelf"),
  };
}
function createSystemGridProps() {
  const spacing = (name, value, theme) =>
    createUnaryUnit(theme, "spacing", 8)(value);
  return {
    gap: prop("gap", spacing),
    columnGap: prop("columnGap", spacing),
    rowGap: prop("rowGap", spacing),
    gridColumn: prop("gridColumn"),
    gridRow: prop("gridRow"),
    gridAutoFlow: prop("gridAutoFlow"),
    gridAutoColumns: prop("gridAutoColumns"),
    gridAutoRows: prop("gridAutoRows"),
    gridTemplateColumns: prop("gridTemplateColumns"),
    gridTemplateRows: prop("gridTemplateRows"),
    gridTemplateAreas: prop("gridTemplateAreas"),
    gridArea: prop("gridArea"),
  };
}
function createSystemPositionProps() {
  return {
    position: prop("position"),
    zIndex: prop(
      "zIndex",
      (name, value, theme) => theme.zIndex?.[name] ?? value,
    ),
    top: pxProp("top"),
    right: pxProp("right"),
    bottom: pxProp("bottom"),
    left: pxProp("left"),
  };
}
function createSystemPaletteProps() {
  const paletteValue = (name, value, theme) =>
    getThemeValue(theme, "palette", value);
  return {
    color: prop("color", paletteValue),
    bgcolor: prop("backgroundColor", paletteValue),
    backgroundColor: prop("backgroundColor", paletteValue),
  };
}
function createSystemSizingProps() {
  const onValue = (name, value, theme) => {
    if (name === "maxWidth") {
      value = theme.breakpoints.values[name] ?? value;
    }
    if (typeof value === "number") {
      value = value > 0 && value <= 1 ? `${value * 100}%` : `${value}px`;
    }
    return value;
  };
  return {
    width: prop("width", onValue),
    maxWidth: prop("maxWidth", onValue),
    minWidth: prop("minWidth", onValue),
    height: prop("height", onValue),
    maxHeight: prop("maxHeight", onValue),
    minHeight: prop("minHeight", onValue),
    boxSizing: prop("boxSizing", onValue),
  };
}
function createSystemBorderProps() {
  const borderValue = (name, value) =>
    typeof value === "number" ? `${value}px solid` : value;
  const paletteValue = (name, value, theme) =>
    getThemeValue(theme, "palette", value);
  return {
    border: prop("border", borderValue),
    borderTop: prop("borderTop", borderValue),
    borderRight: prop("borderRight", borderValue),
    borderBottom: prop("borderBottom", borderValue),
    borderLeft: prop("borderLeft", borderValue),
    borderColor: prop("borderColor", paletteValue),
    borderTopColor: prop("borderTopColor", paletteValue),
    borderRightColor: prop("borderRightColor", paletteValue),
    borderBottomColor: prop("borderBottomColor", paletteValue),
    borderLeftColor: prop("borderLeftColor", paletteValue),
    borderRadius: prop("borderRadius", (name, value, theme) =>
      typeof value === "number"
        ? `${theme.shape.borderRadius * value}px`
        : value,
    ),
  };
}
function createSystemTypographyProps() {
  const typographyValue = (name, value, theme) =>
    getThemeValue(theme, "typography", value);
  return {
    typography: customProp("typography", (name, value, theme) =>
      getThemeValue(theme, "typography", value),
    ),
    fontFamily: prop("fontFamily", typographyValue),
    fontSize: prop("fontSize", (name, value, theme) =>
      asPx(typographyValue(name, value, theme)),
    ),
    fontStyle: prop("fontStyle", typographyValue),
    fontWeight: prop("fontWeight", typographyValue),
    letterSpacing: pxProp("letterSpacing"),
    lineHeight: prop("lineHeight"),
    textAlign: prop("textAlign"),
    textTransform: prop("textTransform"),
  };
}
function createSystemSpacingProps() {
  const spacing = (name, value, theme) => theme.spacing(value);
  const m = "margin";
  const p = "padding";
  return {
    m: prop(m, spacing),
    mt: prop("marginTop", spacing),
    mr: prop("marginRight", spacing),
    mb: prop("marginBottom", spacing),
    ml: prop("marginLeft", spacing),
    mx: mProp(m, dirMap["x"], spacing),
    my: mProp(m, dirMap["y"], spacing),
    margin: prop(m, spacing),
    marginTop: prop("marginTop", spacing),
    marginRight: prop("marginRight", spacing),
    marginBottom: prop("marginBottom", spacing),
    marginLeft: prop("marginLeft", spacing),
    marginX: mProp(m, dirMap["x"], spacing),
    marginY: mProp(m, dirMap["y"], spacing),
    marginInline: mProp(m, ["Inline", "InlineStart"], spacing),
    marginInlineStart: prop("marginInlineStart", spacing),
    marginInlineEnd: prop("marginInlineEnd", spacing),
    marginBlock: mProp(m, ["BlockStart", "BlockEnd"], spacing),
    marginBlockStart: prop("marginBlockStart", spacing),
    marginBlockEnd: prop("marginBlockEnd", spacing),
    p: prop(p, spacing),
    pt: prop("paddingTop", spacing),
    pr: prop("paddingRight", spacing),
    pb: prop("paddingBottom", spacing),
    pl: prop("paddingLeft", spacing),
    px: mProp(p, dirMap["x"], spacing),
    py: mProp(p, dirMap["y"], spacing),
    padding: prop(p, spacing),
    paddingTop: prop("paddingTop", spacing),
    paddingRight: prop("paddingRight", spacing),
    paddingBottom: prop("paddingBottom", spacing),
    paddingLeft: prop("paddingLeft", spacing),
    paddingX: mProp(p, dirMap["x"], spacing),
    paddingY: mProp(p, dirMap["y"], spacing),
    paddingInline: mProp(p, ["Inline", "InlineStart"], spacing),
    paddingInlineStart: prop("paddingInlineStart", spacing),
    paddingInlineEnd: prop("paddingInlineEnd", spacing),
    paddingBlock: mProp(p, ["BlockStart", "BlockEnd"], spacing),
    paddingBlockStart: prop("paddingBlockStart", spacing),
    paddingBlockEnd: prop("paddingBlockEnd", spacing),
  };
}

const systemProps = createSystemProps();
const systemPropNames = Object.keys(systemProps);

function resolveSystemPropsValue(name, value, theme) {
  return systemProps[name](value, theme);
}
function resolveSxPropsValue(name, value, theme) {
  return name in systemProps
    ? resolveSystemPropsValue(name, value, theme)
    : resolveStyledPropsValue(name, value);
}
function resolveSxProps(object, theme) {
  return resolve(object, (name, value) => {
    if (typeof value === "function") {
      value = value(theme);
    }
    if (value === null || value === undefined) {
      return value;
    } else if (typeof value === "object") {
      const sortedCss = {};
      for (const key of theme.breakpoints.keys) {
        sortedCss[theme.breakpoints.up(key)] = {};
      }
      return Object.assign(
        sortedCss,
        handleBreakpoints(
          { theme },
          value,
          (v) => resolveSxPropsValue(name, v, theme) ?? { [name]: v },
        ),
      );
    } else {
      return resolveSxPropsValue(name, value, theme);
    }
  });
}

function r(e) {
  var t,
    f,
    n = "";
  if ("string" == typeof e || "number" == typeof e) n += e;
  else if ("object" == typeof e)
    if (Array.isArray(e))
      for (t = 0; t < e.length; t++)
        e[t] && (f = r(e[t])) && (n && (n += " "), (n += f));
    else for (t in e) e[t] && (n && (n += " "), (n += t));
  return n;
}
function clsx() {
  for (var e, t, f = 0, n = ""; f < arguments.length; )
    (e = arguments[f++]) && (t = r(e)) && (n && (n += " "), (n += t));
  return n;
}

function redefine(
  component,
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  ..._types
) {
  return component;
}
const skipProps = ["ownerState", "theme", "sx", "as"];
function resolveStyles(useTheme, className, styles, inProps) {
  return createMemo(() => {
    const theme = useTheme();
    const ownerState = inProps.ownerState;
    return styles.reduce((result, style) => {
      let styledProps;
      if (typeof style === "function") {
        styledProps = style({
          ownerState,
          theme,
          get sx() {
            return inProps.sx;
          },
          get as() {
            return inProps.as;
          },
          props: inProps,
        });
      } else if (style) {
        styledProps = style;
      }
      if (styledProps)
        result.push(
          resolveStyledProps(styledProps, {
            name: className,
            __resolved: true,
          }),
        );
      return result;
    }, []);
  });
}
function setSuidComponentType(cb, type) {
  cb["__suid"] = type;
}
function getSuidComponentType(input) {
  if (typeof input === "function") return input["__suid"];
}
function createStyled(config) {
  return function styled(Component, options = {}) {
    let cssClassName;
    let className;
    if (options.name) {
      const slot = options.slot || "Root";
      className = cssClassName = `${options.name}-${
        slot.slice(0, 1).toLowerCase() + slot.slice(1)
      }`;
    } else {
      className = `styled-${randomString()}`;
      cssClassName = "css";
    }
    const componentType = getSuidComponentType(Component);
    return function (...styles) {
      function StyledComponent(inProps) {
        let theme;
        const $useTheme = () => {
          const inTheme = inProps.theme;
          if (inTheme) {
            return inTheme;
          } else if (theme) {
            return theme;
          } else {
            return (theme =
              config && config.onUseTheme ? config.onUseTheme() : useTheme$2());
          }
        };
        const [, otherProps] = splitProps(
          inProps,
          options.skipProps || skipProps,
        );
        const inStyles = resolveStyles(
          $useTheme,
          cssClassName,
          styles,
          inProps,
        );
        const inSx = () => {
          const sx = inProps.sx;
          return sx ? (Array.isArray(sx) ? sx : [sx]) : [];
        };
        const $component = () => {
          if (componentType) return Component;
          const as = inProps.as;
          return as ? as : Component;
        };
        const $componentType = componentType
          ? () => componentType
          : createMemo(() => getSuidComponentType($component()));
        const sx = options.skipSx
          ? () => inStyles()
          : () => {
              const theme = $useTheme();
              return [
                ...inStyles(),
                ...inSx().map((sx) =>
                  sx.__resolved ? sx : resolveSxProps(sx, theme),
                ),
              ];
            };
        const styleClassName = createStyle(() =>
          $componentType() ? undefined : sx(),
        );
        return createDynamicComponent(
          $component,
          mergeProps(otherProps, {
            get children() {
              return inProps.children;
            },
            // [review] This property must be omitted on each component individually.
            get component() {
              return $componentType() ? inProps.component : null;
            },
            get as() {
              return componentType ? inProps.as : undefined;
            },
            get sx() {
              return $componentType() ? sx() : undefined;
            },
            get ownerState() {
              return $componentType() === "system"
                ? inProps.ownerState
                : undefined;
            },
            get class() {
              return clsx([
                ...new Set([inProps.class, className, styleClassName()]),
              ]);
            },
          }),
        );
      }
      setSuidComponentType(StyledComponent, "system");
      if (className) StyledComponent.toString = () => `.${className}`;
      return StyledComponent;
    };
  };
}

const skipRootProps = [...skipProps, "classes"];
const styled$1 = createStyled({
  onUseTheme: () => useTheme$1(),
});

// https://github.com/mui/material-ui/blob/master/packages/mui-base/src/composeClasses/composeClasses.ts
function composeClasses(slots, getUtilityClass, classes) {
  const output = {};
  Object.keys(slots).forEach(
    // `Objet.keys(slots)` can't be wider than `T` because we infer `T` from `slots`.
    // @ts-expect-error https://github.com/microsoft/TypeScript/pull/12253#issuecomment-263132208
    (slot) => {
      output[slot] = slots[slot]
        .reduce((acc, key) => {
          if (key) {
            if (classes && classes[key]) {
              acc.push(classes[key]);
            }
            acc.push(getUtilityClass(key));
          }
          return acc;
        }, [])
        .join(" ");
    },
  );
  return output;
}

const $INSPECT = Symbol("solid-inspect");
let inspectionEnabled = false;
function inspect(fn) {
  try {
    inspectionEnabled = true;
    const result = fn();
    return Array.isArray(result) ? result : [result];
  } finally {
    inspectionEnabled = false;
  }
}
function componentTrap(fn) {
  function Component(props) {
    if (inspectionEnabled)
      return {
        Component,
        props,
        $INSPECT,
      };
    return fn(props);
  }
  Object.defineProperty(Component, "name", {
    value: fn.name,
  });
  Component.toString = fn.toString;
  return Component;
}
function isComponentObject(input, component) {
  return (
    !!input &&
    input.$INSPECT === $INSPECT &&
    (!component || input.Component === component)
  );
}
function resolveChildren(children) {
  if (typeof children === "function" && !children.length)
    return resolveChildren(children());
  if (Array.isArray(children)) {
    const results = [];
    for (let i = 0; i < children.length; i++) {
      const result = resolveChildren(children[i]);
      Array.isArray(result)
        ? // eslint-disable-next-line prefer-spread
          results.push.apply(results, result)
        : results.push(result);
    }
    return results;
  }
  return children;
}
function inspectChildren(fn) {
  const children = createMemo(() => inspect(fn));
  const memo = createMemo(() => inspect(() => resolveChildren(children())));
  memo.toArray = () => {
    const c = memo();
    return Array.isArray(c) ? c : c != null ? [c] : [];
  };
  return memo;
}

function createComponentFactory() {
  return function (options) {
    function useClasses(ownerState) {
      const haveSlotClasses = !!options.slotClasses;
      const compose = () => {
        if (!options.slotClasses)
          throw new Error(`'slotClasses' option is not defined`);
        if (!options.utilityClass)
          throw new Error(`'utilityClass' option is not defined`);
        return composeClasses(
          options.slotClasses(ownerState),
          options.utilityClass,
          ownerState["classes"] ?? "",
        );
      };
      const classes = createMutable({});
      if (haveSlotClasses)
        createComputed(() => {
          const result = compose();
          batch(() => {
            for (const slot in result) classes[slot] = result[slot];
          });
        });
      return classes;
    }
    function splitInProps(allProps) {
      const [props, otherProps] = splitProps(allProps, options.selfPropNames);
      return { allProps, props, otherProps };
    }
    function useThemeProps(input) {
      return useThemeProps$1({
        propDefaults: input.propDefaults || options.propDefaults,
        props: input.props,
        name: options.name,
      });
    }
    function useProps(props) {
      const themeProps = useThemeProps({ props });
      return splitInProps(themeProps);
    }
    function defineComponent(cb, styled = true) {
      cb = componentTrap(cb);
      cb.toString = () => `.${options.name}-root`;
      if (styled) setSuidComponentType(cb, "base");
      return cb;
    }
    function component(cb) {
      const Component = defineComponent(function Component(inProps) {
        const { allProps, otherProps, props } = useProps(inProps);
        const classes =
          options.autoCallUseClasses ?? true ? useClasses(allProps) : {};
        return cb({
          allProps,
          otherProps,
          props,
          classes,
        });
      });
      Object.defineProperty(Component, "name", { value: cb.name });
      return Component;
    }
    return {
      name: options.name,
      selfPropNames: options.selfPropNames,
      component,
      defineComponent,
      useClasses,
      useThemeProps,
      useProps,
      splitInProps,
    };
  };
}

function defineComponent(cb) {
  return cb;
}

function splitSxProps(props) {
  const [systemProps, sxProps, otherProps] = splitProps(
    props,
    systemPropNames,
    ["sx"],
  );
  const sx = () => {
    const sx = sxProps.sx;
    if (sx) {
      if (Array.isArray(sx)) {
        return [systemProps, ...sx];
      } else {
        return mergeProps(systemProps, sx);
      }
    } else {
      return systemProps;
    }
  };
  return [sx, otherProps];
}
function extendSxProp(props) {
  const [sx, otherProps] = splitSxProps(props);
  return mergeProps(otherProps, {
    get sx() {
      return sx();
    },
  });
}

const styled = createStyled();

const BoxRoot = styled("div", {
  name: "MuiBox",
  slot: "Root",
  skipProps: [...skipProps, "component"],
})();
const Box$1 = defineComponent(function Box(inProps) {
  inProps = extendSxProp(inProps);
  return createComponent(
    BoxRoot,
    mergeProps(inProps, {
      get as() {
        return inProps.as ?? inProps.component;
      },
    }),
  );
});
Box$1.toString = BoxRoot.toString;

const $$1w = createComponentFactory()({
  name: "MuiBox",
  selfPropNames: [],
  utilityClass: (slot) => `MuiBox-${slot}`,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const Box = $$1w.component(function Box({ otherProps, classes }) {
  const theme = useTheme$1();
  return createComponent(
    Box$1,
    mergeProps(
      {
        theme: theme,
      },
      otherProps,
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
      },
    ),
  );
});

const _tmpl$$n = /*#__PURE__*/ template(`<span>`);
const $$1v = createComponentFactory()({
  name: "MuiRipple",
  selfPropNames: [
    "class",
    "classes",
    "pulsate",
    "rippleX",
    "rippleY",
    "rippleSize",
    "in",
    "onExited",
    "timeout",
  ],
});

/**
 * @ignore - internal component.
 */
const Ripple = $$1v.component(function Ripple({ props, otherProps }) {
  const [leaving, setLeaving] = createSignal(false);
  const rippleClassName = () =>
    clsx(
      props.class,
      props.classes.ripple,
      props.classes.rippleVisible,
      props.classes.ripplePulsate && {
        [props.classes.ripplePulsate]: props.pulsate,
      },
    );
  const rippleStyles = () => ({
    width: `${props.rippleSize}px`,
    height: `${props.rippleSize}px`,
    top: `${-(props.rippleSize / 2) + props.rippleY}px`,
    left: `${-(props.rippleSize / 2) + props.rippleX}px`,
  });
  const childClassName = () =>
    clsx(
      props.classes.child,
      props.classes.childLeaving && {
        [props.classes.childLeaving]: leaving(),
      },
      props.classes.childPulsate && {
        [props.classes.childPulsate]: props.pulsate,
      },
    );
  createEffect(() => {
    if (!props.in && !leaving()) {
      setLeaving(true);
    }
  });
  let timeoutId;
  createEffect(() => {
    if (!props.in && props.onExited) {
      clearTimeout(timeoutId);
      timeoutId = window.setTimeout(props.onExited, props.timeout);
    }
  });
  onCleanup(() => clearTimeout(timeoutId));
  return createComponent(Box, {
    as: "span",
    get ["class"]() {
      return rippleClassName();
    },
    get style() {
      return rippleStyles();
    },
    get sx() {
      return otherProps.sx;
    },
    get children() {
      const _el$ = _tmpl$$n();
      createRenderEffect(() => className(_el$, childClassName()));
      return _el$;
    },
  });
});

const defaultGenerator = (componentName) => componentName;
const createClassNameGenerator = () => {
  let generate = defaultGenerator;
  return {
    configure(generator) {
      generate = generator;
    },
    generate(componentName) {
      return generate(componentName);
    },
    reset() {
      generate = defaultGenerator;
    },
  };
};
const ClassNameGenerator = createClassNameGenerator();
const globalStateClassesMapping = {
  active: "Mui-active",
  checked: "Mui-checked",
  completed: "Mui-completed",
  disabled: "Mui-disabled",
  error: "Mui-error",
  expanded: "Mui-expanded",
  focused: "Mui-focused",
  focusVisible: "Mui-focusVisible",
  required: "Mui-required",
  selected: "Mui-selected",
};
function generateUtilityClass(componentName, slot) {
  const globalStateClass = globalStateClassesMapping[slot];
  return (
    globalStateClass || `${ClassNameGenerator.generate(componentName)}-${slot}`
  );
}

function generateUtilityClasses(componentName, slots) {
  const result = {};
  slots.forEach((slot) => {
    result[slot] = generateUtilityClass(componentName, slot);
  });
  return result;
}

const touchRippleClasses = generateUtilityClasses("MuiTouchRipple", [
  "root",
  "ripple",
  "rippleVisible",
  "ripplePulsate",
  "child",
  "childLeaving",
  "childPulsate",
]);

function createRef(input) {
  const cb = (e) => {
    cb.ref = cb.current = e;
    if (typeof input === "function") {
      const inputResult = input();
      if (typeof inputResult === "function") {
        inputResult(e);
      }
    } else if (typeof input?.ref === "function") {
      input.ref(e);
    }
  };
  return cb;
}

function createElementRef(props) {
  return createRef(props);
}

const $$1u = createComponentFactory()({
  name: "MuiTouchRipple",
  selfPropNames: ["center", "classes", "ref"],
});

//const DURATION = 10_000; //550;
const DURATION = 550;
const DELAY_RIPPLE = 80;
const TouchRippleRoot = styled$1("span", {
  name: "MuiTouchRipple",
  slot: "Root",
})({
  overflow: "hidden",
  pointerEvents: "none",
  position: "absolute",
  zIndex: 0,
  top: 0,
  right: 0,
  bottom: 0,
  left: 0,
  borderRadius: "inherit",
});

// This `styled()` function invokes keyframes. `styled-components` only supports keyframes
// in string templates. Do not convert these styles in JS object as it will break.
const TouchRippleRipple = styled$1(Ripple, {
  name: "MuiTouchRipple",
  slot: "Ripple",
})(({ theme }) => ({
  position: "absolute",
  "@keyframes animation-enter-$id": {
    0: {
      transform: "scale(0)",
      opacity: 0.1,
    },
    100: {
      transform: "scale(1)",
      opacity: 0.3,
    },
  },
  "@keyframes animation-exit-$id": {
    0: {
      opacity: 1,
    },
    100: {
      opacity: 0,
    },
  },
  "@keyframes animation-pulsate-$id": {
    0: {
      transform: "scale(1)",
    },
    50: {
      transform: "scale(0.92)",
    },
    100: {
      transform: "scale(1)",
    },
  },
  [`&.${touchRippleClasses.rippleVisible}`]: {
    opacity: "0.3",
    transform: "scale(1)",
    animationName: `animation-enter-$id`,
    animationDuration: `${DURATION}ms`,
    animationTimingFunction: theme.transitions.easing.easeInOut,
  },
  [`&.${touchRippleClasses.ripplePulsate}`]: {
    animationDuration: `${theme.transitions.duration.shorter}ms`,
  },
  [`& .${touchRippleClasses.child}`]: {
    opacity: 1,
    display: "block",
    width: "100%",
    height: "100%",
    borderRadius: "50%",
    backgroundColor: "currentColor",
  },
  [`& .${touchRippleClasses.childLeaving}`]: {
    opacity: 0,
    animationName: `animation-exit-$id`,
    animationDuration: `${DURATION}ms`,
    animationTimingFunction: `${theme.transitions.easing.easeInOut}`,
  },
  [`& .${touchRippleClasses.childPulsate}`]: {
    position: "absolute",
    left: "0px",
    top: 0,
    animationName: `animation-pulsate-$id`,
    animationDuration: "2500ms",
    animationTimingFunction: `${theme.transitions.easing.easeInOut}`,
    animationIterationCount: "infinite",
    animationDelay: "200ms",
  },
}));

/**
 * @ignore - internal component.
 *
 * TODO v5: Make private
 */

const TouchRipple = $$1u.defineComponent(function TouchRipple(inProps) {
  const props = mergeProps(
    {
      classes: {},
    },
    inProps,
  );
  const [, otherProps] = splitProps(props, ["center", "classes", "ref"]);
  let counter = 0;
  const [ripples, setRipples] = createSignal([]);
  const inState = createMutable({});
  let rippleCallback;
  // Used to filter out mouse emulated events on mobile.
  let ignoringMouseDown = false;
  // We use a timer in order to only show the ripples for touch "click" like events.
  // We don't want to display the ripple for touch scroll events.
  let startTimer;
  // This is the hook called once the previous timeout is ready.
  let startTimerCommit;
  const container = createElementRef();
  onCleanup(() => {
    if (startTimer) clearTimeout(startTimer);
  });
  createEffect(() => {
    ripples();
    if (rippleCallback) {
      rippleCallback();
      rippleCallback = undefined;
    }
  });
  const startCommit = (params) => {
    const id = counter++;
    inState[id] = true;
    setRipples((oldRipples) => [
      ...oldRipples,
      {
        id,
        params,
      },
    ]);
    rippleCallback = params.cb;
  };
  const start = (
    event,
    options = {
      pulsate: false,
      center: props.center,
    },
    cb,
  ) => {
    options = mergeProps(options, {
      center: options.center || options.pulsate,
    });
    if (event.type === "mousedown" && ignoringMouseDown) {
      ignoringMouseDown = false;
      return;
    }
    if (event.type === "touchstart") {
      ignoringMouseDown = true;
    }
    const rect = container.ref
      ? container.ref.getBoundingClientRect()
      : {
          width: 0,
          height: 0,
          left: 0,
          top: 0,
        };

    // Get the size of the ripple
    let rippleX;
    let rippleY;
    let rippleSize;
    if (
      options.center ||
      (event.clientX === 0 && event.clientY === 0) ||
      (!event.clientX && !event.touches)
    ) {
      rippleX = Math.round(rect.width / 2);
      rippleY = Math.round(rect.height / 2);
    } else {
      const { clientX, clientY } = event.touches ? event.touches[0] : event;
      rippleX = Math.round(clientX - rect.left);
      rippleY = Math.round(clientY - rect.top);
    }
    if (options.center) {
      rippleSize = Math.sqrt((2 * rect.width ** 2 + rect.height ** 2) / 3);

      // For some reason the animation is broken on Mobile Chrome if the size is even.
      if (rippleSize % 2 === 0) {
        rippleSize += 1;
      }
    } else {
      const sizeX =
        Math.max(
          Math.abs((container.ref ? container.ref.clientWidth : 0) - rippleX),
          rippleX,
        ) *
          2 +
        2;
      const sizeY =
        Math.max(
          Math.abs((container.ref ? container.ref.clientHeight : 0) - rippleY),
          rippleY,
        ) *
          2 +
        2;
      rippleSize = Math.sqrt(sizeX ** 2 + sizeY ** 2);
    }

    // Touche devices
    if (event.touches) {
      // check that this isn't another touchstart due to multitouch
      // otherwise we will only clear a single timer when unmounting while two
      // are running
      if (!startTimerCommit) {
        // Prepare the ripple effect.
        startTimerCommit = () => {
          startCommit({
            pulsate: options.pulsate,
            rippleX,
            rippleY,
            rippleSize,
            cb,
          });
        };
        // Delay the execution of the ripple effect.
        startTimer = setTimeout(() => {
          if (startTimerCommit) {
            startTimerCommit();
            startTimerCommit = undefined;
          }
        }, DELAY_RIPPLE); // We have to make a tradeoff with this value.
      }
    } else {
      startCommit({
        pulsate: options.pulsate,
        rippleX,
        rippleY,
        rippleSize,
        cb,
      });
    }
  };
  const pulsate = () =>
    start(
      {},
      {
        pulsate: true,
      },
    );
  const stop = (event, cb) => {
    clearTimeout(startTimer);

    // The touch interaction occurs too quickly.
    // We still want to show ripple effect.
    if (event.type === "touchend" && startTimerCommit) {
      startTimerCommit();
      startTimerCommit = undefined;
      startTimer = setTimeout(() => {
        stop(event, cb);
      });
      return;
    }
    for (const id in inState) inState[id] = false;
    startTimerCommit = undefined;
    rippleCallback = cb;
  };
  const actions = {
    pulsate,
    start,
    stop,
  };
  if (typeof props.ref === "function") {
    props.ref(actions);
  }
  return createComponent(
    TouchRippleRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(
            props.classes.root,
            touchRippleClasses.root,
            otherProps.class,
          );
        },
        ref: container,
      },
      otherProps,
      {
        get children() {
          return createComponent(For, {
            get each() {
              return ripples();
            },
            children: (data) =>
              createComponent(TouchRippleRipple, {
                get ["in"]() {
                  return inState[data.id];
                },
                onExited: () => {
                  setRipples((oldRipples) =>
                    oldRipples.filter((v) => v.id !== data.id),
                  );
                  delete inState[data.id];
                },
                get classes() {
                  return {
                    ripple: clsx(
                      props.classes.ripple,
                      touchRippleClasses.ripple,
                    ),
                    rippleVisible: clsx(
                      props.classes.rippleVisible,
                      touchRippleClasses.rippleVisible,
                    ),
                    ripplePulsate: clsx(
                      props.classes.ripplePulsate,
                      touchRippleClasses.ripplePulsate,
                    ),
                    child: clsx(props.classes.child, touchRippleClasses.child),
                    childLeaving: clsx(
                      props.classes.childLeaving,
                      touchRippleClasses.childLeaving,
                    ),
                    childPulsate: clsx(
                      props.classes.childPulsate,
                      touchRippleClasses.childPulsate,
                    ),
                  };
                },
                timeout: DURATION,
                get pulsate() {
                  return data.params.pulsate;
                },
                get rippleX() {
                  return data.params.rippleX;
                },
                get rippleY() {
                  return data.params.rippleY;
                },
                get rippleSize() {
                  return data.params.rippleSize;
                },
              }),
          });
        },
      },
    ),
  );
});

function getButtonBaseUtilityClass(slot) {
  return generateUtilityClass("MuiButtonBase", slot);
}
const buttonBaseClasses = generateUtilityClasses("MuiButtonBase", [
  "root",
  "disabled",
  "focusVisible",
]);

const $$1t = createComponentFactory()({
  name: "MuiButtonBase",
  selfPropNames: [
    "LinkComponent",
    "TouchRippleProps",
    "action",
    "centerRipple",
    "children",
    "classes",
    "disableRipple",
    "disableRipple",
    "disableTouchRipple",
    "disabled",
    "focusRipple",
    "focusVisibleClassName",
    "onFocusVisible",
    "tabIndex",
    "touchRippleRef",
  ],
  autoCallUseClasses: false,
  utilityClass: getButtonBaseUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.disabled && "disabled",
      ownerState.focusVisible && "focusVisible",
    ],
  }),
});
const ButtonBaseRoot = styled$1("button", {
  name: "MuiButtonBase",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  display: "inline-flex",
  alignItems: "center",
  justifyContent: "center",
  position: "relative",
  boxSizing: "border-box",
  ["WebkitTapHighlightColor"]: "transparent",
  backgroundColor: "transparent",
  // Reset default value
  // We disable the focus ring for mouse, touch and keyboard users.
  outline: 0,
  border: 0,
  margin: 0,
  // Remove the margin in Safari
  borderRadius: 0,
  padding: 0,
  // Remove the padding in Firefox
  cursor: "pointer",
  userSelect: "none",
  verticalAlign: "middle",
  ["MozAppearance"]: "none",
  // Reset
  ["WebkitAppearance"]: "none",
  // Reset
  textDecoration: "none",
  // So we take precedent over the style of a native <a /> element.
  color: "inherit",
  "&::-moz-focus-inner": {
    borderStyle: "none", // Remove Firefox dotted outline.
  },

  [`&.${buttonBaseClasses.disabled}`]: {
    pointerEvents: "none",
    // Disable link interactions
    cursor: "default",
  },
  "@media print": {
    colorAdjust: "exact",
  },
});

/**
 * `ButtonBase` contains as few styles as possible.
 * It aims to be a simple building block for creating a button.
 * It contains a load of style reset and some focus/ripple logic.
 *
 * Demos:
 *
 * - [Buttons](https://mui.com/components/buttons/)
 *
 * API:
 *
 * - [ButtonBase API](https://mui.com/api/button-base/)
 */
const ButtonBase = $$1t.defineComponent(function ButtonBase(inProps) {
  const theme = useTheme$1();
  const props = mergeProps(
    {
      component: "button",
      LinkComponent: "a",
      tabIndex: 0,
    },
    theme.components?.[$$1t.name]?.defaultProps,
    inProps,
  );
  const button = createRef(inProps);
  const ripple = createRef(() => props.touchRippleRef);
  const focus = useIsFocusVisible();
  let keydown = false;
  const [focusVisible, setFocusVisible] = createSignal(false);
  const [mountedState, setMountedState] = createSignal(false);
  const [, otherProps] = splitProps(props, [
    "LinkComponent",
    "TouchRippleProps",
    "action",
    "centerRipple",
    "children",
    "classes",
    "disableRipple",
    "disableRipple",
    "disableTouchRipple",
    "disabled",
    "focusRipple",
    "focusVisibleClassName",
    "onFocusVisible",
    "tabIndex",
    "touchRippleRef",
  ]);
  const ownerState = {
    get classes() {
      return props.classes;
    },
    get disabled() {
      return props.disabled || false;
    },
    get focusVisible() {
      return focusVisible();
    },
  };
  const classes = $$1t.useClasses(ownerState);
  onMount(() => {
    setMountedState(true);
  });
  createEffect(() => {
    if (props.disabled && focusVisible()) {
      setFocusVisible(false);
    }
  });
  createEffect(() => {
    if (focusVisible() && props.focusRipple && !props.disableRipple) {
      ripple.ref.pulsate();
    }
  });
  function useRippleHandler(
    rippleAction,
    eventCallback,
    skipRippleAction = props.disableTouchRipple,
  ) {
    return (event) => {
      if (typeof eventCallback === "function") {
        eventCallback(event);
      }
      const ignore = skipRippleAction;
      if (!ignore && ripple.ref) {
        ripple.ref[rippleAction](event);
      }
      return true;
    };
  }
  const handleMouseDown = useRippleHandler("start", props.onMouseDown);
  const handleContextMenu = useRippleHandler("stop", props.onContextMenu);
  const handleDragLeave = useRippleHandler("stop", props.onDragLeave);
  const handleMouseUp = useRippleHandler("stop", props.onMouseUp);
  const handleMouseLeave = useRippleHandler("stop", (event) => {
    if (focusVisible()) {
      event.preventDefault();
    }
    if (typeof props.onMouseLeave === "function") {
      props.onMouseLeave(event);
    }
  });
  const handleTouchStart = useRippleHandler("start", props.onTouchStart);
  const handleTouchEnd = useRippleHandler("stop", props.onTouchEnd);
  const handleTouchMove = useRippleHandler("stop", props.onTouchMove);
  const handleBlur = useRippleHandler(
    "stop",
    (event) => {
      focus.onBlur(event);
      if (focus.isFocusVisibleRef.current === false) {
        setFocusVisible(false);
      }
      if (typeof props.onFocusOut === "function") {
        props.onFocusOut(event);
      }
    },
    false,
  );
  const handleFocus = (event) => {
    // Fix for https://github.com/facebook/react/issues/7769
    if (!button.ref) {
      button.ref = event.currentTarget;
    }
    focus.onFocus(event);
    if (focus.isFocusVisibleRef.current === true) {
      setFocusVisible(true);
      if (props.onFocusVisible) {
        props.onFocusVisible(event);
      }
    }
    if (typeof props.onFocusIn === "function") {
      props.onFocusIn(event);
    }
  };
  const isNonNativeButton = () => {
    return (
      props.component &&
      props.component !== "button" &&
      !(button.ref.tagName === "A" && button.ref.hasAttribute("href"))
    );
  };

  /**
   * IE11 shim for https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/repeat
   */

  const handleKeyDown = (event) => {
    // Check if key is already down to avoid repeats being counted as multiple activations
    if (
      props.focusRipple &&
      !keydown &&
      focusVisible() &&
      ripple.ref &&
      event.key === " "
    ) {
      keydown = true;
      ripple.ref.stop(event, () => {
        ripple.ref.start(event);
      });
    }
    if (
      event.target === event.currentTarget &&
      isNonNativeButton() &&
      event.key === " "
    ) {
      event.preventDefault();
    }
    if (typeof props.onKeyDown === "function") {
      props.onKeyDown(event);
    }

    // Keyboard accessibility for non interactive elements
    if (
      event.target === event.currentTarget &&
      isNonNativeButton() &&
      event.key === "Enter" &&
      !props.disabled
    ) {
      event.preventDefault();
      if (typeof props.onClick === "function") {
        props.onClick(event);
      }
    }
  };
  const handleKeyUp = (event) => {
    // calling preventDefault in keyUp on a <button> will not dispatch a click event if Space is pressed
    // https://codesandbox.io/s/button-keyup-preventdefault-dn7f0
    if (
      props.focusRipple &&
      event.key === " " &&
      ripple.ref &&
      focusVisible() &&
      !event.defaultPrevented
    ) {
      keydown = false;
      ripple.ref.stop(event, () => {
        ripple.ref.pulsate(event);
      });
    }
    if (typeof props.onKeyUp === "function") {
      props.onKeyUp(event);
    }

    // Keyboard accessibility for non interactive elements
    if (
      typeof props.onClick === "function" &&
      event.target === event.currentTarget &&
      isNonNativeButton() &&
      event.key === " " &&
      !event.defaultPrevented
    ) {
      props.onClick(event);
    }
  };
  const ComponentProp = createMemo(() => {
    let result = props.component;
    if (result === "button" && (props.href || props.to)) {
      result = props.LinkComponent;
    }
    return result;
  });
  const isButtonComponent = () => ComponentProp() === "button";
  const enableTouchRipple = () =>
    mountedState() && !props.disableRipple && !props.disabled;
  // [non-reactive root]
  const touchRipple = props.TouchRippleProps;
  return createComponent(
    ButtonBaseRoot,
    mergeProps(
      {
        get type() {
          return isButtonComponent()
            ? props.type === undefined
              ? "button"
              : props.type
            : undefined;
        },
        get disabled() {
          return isButtonComponent() ? props.disabled : undefined;
        },
        get role() {
          return !isButtonComponent() && props.href && !props.to
            ? "button"
            : undefined;
        },
        get ["aria-disabled"]() {
          return !isButtonComponent() && props.disabled ? true : undefined;
        },
      },
      otherProps,
      {
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
        ownerState: ownerState,
        onFocusOut: handleBlur,
        get onClick() {
          return props.onClick;
        },
        onContextMenu: handleContextMenu,
        onFocusIn: handleFocus,
        onKeyDown: handleKeyDown,
        onKeyUp: handleKeyUp,
        onMouseDown: handleMouseDown,
        onMouseLeave: handleMouseLeave,
        onMouseUp: handleMouseUp,
        onDragLeave: handleDragLeave,
        onTouchEnd: handleTouchEnd,
        onTouchMove: handleTouchMove,
        onTouchStart: handleTouchStart,
        ref: button,
        get tabIndex() {
          return props.disabled ? -1 : props.tabIndex;
        },
        get as() {
          return ComponentProp();
        },
        get children() {
          return [
            createMemo(() => props.children),
            createComponent(Show, {
              get when() {
                return enableTouchRipple();
              },
              get children() {
                return createComponent(
                  TouchRipple,
                  mergeProps(
                    {
                      ref: (ref) => {
                        ripple(ref);
                      },
                      get center() {
                        return props.centerRipple;
                      },
                    },
                    touchRipple,
                  ),
                );
              },
            }),
          ];
        },
      },
    ),
  );
});

function getIconButtonUtilityClass(slot) {
  return generateUtilityClass("MuiIconButton", slot);
}
const iconButtonClasses = generateUtilityClasses("MuiIconButton", [
  "root",
  "disabled",
  "colorInherit",
  "colorPrimary",
  "colorSecondary",
  "edgeStart",
  "edgeEnd",
  "sizeSmall",
  "sizeMedium",
  "sizeLarge",
]);

const $$1s = createComponentFactory()({
  name: "MuiIconButton",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disableFocusRipple",
    "disabled",
    "edge",
    "size",
  ],
  propDefaults: ({ set }) =>
    set({
      edge: false,
      color: "default",
      disabled: false,
      disableFocusRipple: false,
      size: "medium",
    }),
  utilityClass: getIconButtonUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.disabled && "disabled",
      ownerState.color !== "default" && `color${capitalize(ownerState.color)}`,
      ownerState.edge && `edge${capitalize(ownerState.edge)}`,
      `size${capitalize(ownerState.size)}`,
    ],
  }),
});
const IconButtonRoot = styled$1(ButtonBase, {
  name: "MuiIconButton",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.color !== "default" &&
        styles[`color${capitalize(ownerState.color)}`],
      ownerState.edge && styles[`edge${capitalize(ownerState.edge)}`],
      styles[`size${capitalize(ownerState.size)}`],
    ];
  },
})(
  ({ theme, ownerState }) => ({
    textAlign: "center",
    flex: "0 0 auto",
    fontSize: theme.typography.pxToRem(24),
    padding: 8,
    borderRadius: "50%",
    overflow: "visible",
    // Explicitly set the default value to solve a bug on IE11.
    color: theme.palette.action.active,
    transition: theme.transitions.create("background-color", {
      duration: theme.transitions.duration.shortest,
    }),
    ...(!ownerState.disableRipple && {
      "&:hover": {
        backgroundColor: alpha(
          theme.palette.action.active,
          theme.palette.action.hoverOpacity,
        ),
        // Reset on touch devices, it doesn't add specificity
        "@media (hover: none)": {
          backgroundColor: "transparent",
        },
      },
    }),
    ...(ownerState.edge === "start" && {
      marginLeft: ownerState.size === "small" ? -3 : -12,
    }),
    ...(ownerState.edge === "end" && {
      marginRight: ownerState.size === "small" ? -3 : -12,
    }),
  }),
  ({ theme, ownerState }) => ({
    ...(ownerState.color === "inherit" && {
      color: "inherit",
    }),
    ...(ownerState.color !== "inherit" &&
      ownerState.color !== "default" && {
        color: theme.palette[ownerState.color].main,
        ...(!ownerState.disableRipple && {
          "&:hover": {
            backgroundColor: alpha(
              theme.palette[ownerState.color].main,
              theme.palette.action.hoverOpacity,
            ),
            // Reset on touch devices, it doesn't add specificity
            "@media (hover: none)": {
              backgroundColor: "transparent",
            },
          },
        }),
      }),
    ...(ownerState.size === "small" && {
      padding: 5,
      fontSize: theme.typography.pxToRem(18),
    }),
    ...(ownerState.size === "large" && {
      padding: 12,
      fontSize: theme.typography.pxToRem(28),
    }),
    [`&.${iconButtonClasses.disabled}`]: {
      backgroundColor: "transparent",
      color: theme.palette.action.disabled,
    },
  }),
);

/**
 * Refer to the [Icons](https://mui.com/components/icons/) section of the documentation
 * regarding the available icon options.
 *
 * Demos:
 *
 * - [Buttons](https://mui.com/components/buttons/)
 *
 * API:
 *
 * - [IconButton API](https://mui.com/api/icon-button/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */
const IconButton = $$1s.component(function IconButton({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    IconButtonRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        centerRipple: true,
        get focusRipple() {
          return !props.disableFocusRipple;
        },
        get disabled() {
          return props.disabled;
        },
        ownerState: allProps,
      },
      otherProps,
      {
        get children() {
          return props.children;
        },
      },
    ),
  );
});

function getPaperUtilityClass(slot) {
  return generateUtilityClass("MuiPaper", slot);
}
generateUtilityClasses("MuiPaper", [
  "root",
  "rounded",
  "outlined",
  "elevation",
  "elevation0",
  "elevation1",
  "elevation2",
  "elevation3",
  "elevation4",
  "elevation5",
  "elevation6",
  "elevation7",
  "elevation8",
  "elevation9",
  "elevation10",
  "elevation11",
  "elevation12",
  "elevation13",
  "elevation14",
  "elevation15",
  "elevation16",
  "elevation17",
  "elevation18",
  "elevation19",
  "elevation20",
  "elevation21",
  "elevation22",
  "elevation23",
  "elevation24",
]);

const $$1r = createComponentFactory()({
  name: "MuiPaper",
  selfPropNames: ["children", "classes", "elevation", "square", "variant"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
      elevation: 1,
      square: false,
      variant: "elevation",
    }),
  utilityClass: getPaperUtilityClass,
  slotClasses: (o) => ({
    root: [
      "root",
      o.variant,
      !o.square && "rounded",
      o.variant === "elevation" && `elevation${o.elevation}`,
    ],
  }),
});

// Inspired by https://github.com/material-components/material-components-ios/blob/bca36107405594d5b7b16265a5b0ed698f85a5ee/components/Elevation/src/UIColor%2BMaterialElevation.m#L61
const getOverlayAlpha = (elevation) => {
  let alphaValue;
  if (elevation < 1) {
    alphaValue = 5.11916 * elevation ** 2;
  } else {
    alphaValue = 4.5 * Math.log(elevation + 1) + 2;
  }
  return Number((alphaValue / 100).toFixed(2));
};
const PaperRoot = styled$1("div", {
  name: "MuiPaper",
  slot: "Root",
})(({ theme, ownerState }) => ({
  backgroundColor: theme.palette.background.paper,
  color: theme.palette.text.primary,
  transition: theme.transitions.create("box-shadow"),
  ...(!ownerState.square && {
    borderRadius: theme.shape.borderRadius,
  }),
  ...(ownerState.variant === "outlined" && {
    border: `1px solid ${theme.palette.divider}`,
  }),
  ...(ownerState.variant === "elevation" && {
    boxShadow: theme.shadows[ownerState.elevation],
    ...(theme.palette.mode === "dark" && {
      backgroundImage: `linear-gradient(${alpha(
        "#fff",
        getOverlayAlpha(ownerState.elevation),
      )}, ${alpha("#fff", getOverlayAlpha(ownerState.elevation))})`,
    }),
  }),
}));

/**
 *
 * Demos:
 *
 * - [Cards](https://mui.com/components/cards/)
 * - [Paper](https://mui.com/components/paper/)
 *
 * API:
 *
 * - [Paper API](https://mui.com/api/paper/)
 */

const Paper = $$1r.component(function Paper({
  allProps,
  props,
  otherProps,
  classes,
}) {
  return createComponent(
    PaperRoot,
    mergeProps(otherProps, {
      ownerState: allProps,
      get ["class"]() {
        return clsx(classes.root, allProps.class);
      },
      get children() {
        return props.children;
      },
    }),
  );
});

const SvgIconContext = createContext();

function getSvgIconUtilityClass(slot) {
  return generateUtilityClass("MuiSvgIcon", slot);
}
generateUtilityClasses("MuiSvgIcon", [
  "root",
  "colorPrimary",
  "colorSecondary",
  "colorAction",
  "colorError",
  "colorDisabled",
  "fontSizeInherit",
  "fontSizeSmall",
  "fontSizeMedium",
  "fontSizeLarge",
]);

const _tmpl$$m = /*#__PURE__*/ template(`<title>`);
const $$1q = createComponentFactory()({
  name: "MuiSvgIcon",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "fontSize",
    "htmlColor",
    "inheritViewBox",
    "shapeRendering",
    "titleAccess",
    "viewBox",
  ],
  propDefaults: ({ set }) => {
    const context = useContext(SvgIconContext);
    return set({
      component: "svg",
      color: "inherit",
      get fontSize() {
        return context?.fontSize ?? "medium";
      },
      inheritViewBox: false,
      viewBox: "0 0 24 24",
    });
  },
  utilityClass: getSvgIconUtilityClass,
  slotClasses: (o) => ({
    root: [
      "root",
      o.color !== "inherit" && `color${capitalize(o.color)}`,
      `fontSize${capitalize(o.fontSize)}`,
    ],
  }),
});
const SvgIconRoot = styled$1("svg", {
  name: "MuiSvgIcon",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.color !== "inherit" &&
        styles[`color${capitalize(ownerState.color)}`],
      styles[`fontSize${capitalize(ownerState.fontSize)}`],
    ];
  },
})(({ theme, ownerState }) => ({
  userSelect: "none",
  width: "1em",
  height: "1em",
  display: "inline-block",
  fill: "currentColor",
  flexShrink: 0,
  transition: theme.transitions?.create?.("fill", {
    duration: theme.transitions?.duration?.shorter,
  }),
  fontSize: {
    inherit: "inherit",
    small: theme.typography?.pxToRem?.(20) || "1.25rem",
    medium: theme.typography?.pxToRem?.(24) || "1.5rem",
    large: theme.typography?.pxToRem?.(35) || "2.1875",
  }[ownerState.fontSize],
  // TODO v5 deprecate, v6 remove for sx
  color:
    theme.palette?.[ownerState.color]?.main ??
    {
      action: theme.palette?.action?.active,
      disabled: theme.palette?.action?.disabled,
      inherit: undefined,
    }[ownerState.color],
}));

/**
 *
 * Demos:
 *
 * - [Icons](https://mui.com/components/icons/)
 * - [Material Icons](https://mui.com/components/material-icons/)
 *
 * API:
 *
 * - [SvgIcon API](https://mui.com/api/svg-icon/)
 */

const SvgIcon = $$1q.component(function SvgIcon({
  allProps,
  props,
  otherProps,
  classes,
}) {
  return createComponent(
    SvgIconRoot,
    mergeProps(
      {
        get ["aria-hidden"]() {
          return props.titleAccess ? undefined : true;
        },
        get role() {
          return props.titleAccess ? "img" : undefined;
        },
        get viewBox() {
          return !props.inheritViewBox ? props.viewBox : undefined;
        },
      },
      {
        ["focusable"]: "false",
      },
      {
        get color() {
          return props.htmlColor;
        },
      },
      otherProps,
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        ownerState: allProps,
        get children() {
          return [
            createMemo(() => props.children),
            createComponent(Show, {
              get when() {
                return props.titleAccess;
              },
              children: (titleAccess) =>
                (() => {
                  const _el$ = _tmpl$$m();
                  insert(_el$, titleAccess);
                  return _el$;
                })(),
            }),
          ];
        },
      },
    ),
  );
});

function createSvgIcon(path, displayName) {
  const Component = (props) =>
    createComponent(
      SvgIcon,
      mergeProps(
        {
          "data-testid": `${displayName}Icon`,
        },
        props,
        {
          get children() {
            return path();
          },
        },
      ),
    );
  return Component;
}

const _tmpl$$l = /*#__PURE__*/ template(
  `<svg><path d="M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 *
 * Alias to `Clear`.
 */
const CloseIcon = createSvgIcon(() => _tmpl$$l(), "Close");

const _tmpl$$k = /*#__PURE__*/ template(
  `<svg><path d="M11 15h2v2h-2zm0-8h2v6h-2zm.99-5C6.47 2 2 6.48 2 12s4.47 10 9.99 10C17.52 22 22 17.52 22 12S17.52 2 11.99 2zM12 20c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const ErrorOutlineIcon = createSvgIcon(() => _tmpl$$k(), "ErrorOutline");

const _tmpl$$j = /*#__PURE__*/ template(
  `<svg><path d="M11,9H13V7H11M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20, 12C20,16.41 16.41,20 12,20M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10, 10 0 0,0 12,2M11,17H13V11H11V17Z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const InfoOutlinedIcon = createSvgIcon(() => _tmpl$$j(), "InfoOutlined");

const _tmpl$$i = /*#__PURE__*/ template(
  `<svg><path d="M12 5.99L19.53 19H4.47L12 5.99M12 2L1 21h22L12 2zm1 14h-2v2h2v-2zm0-6h-2v4h2v-4z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const ReportProblemOutlinedIcon = createSvgIcon(
  () => _tmpl$$i(),
  "ReportProblemOutlined",
);

const _tmpl$$h = /*#__PURE__*/ template(
  `<svg><path d="M20,12A8,8 0 0,1 12,20A8,8 0 0,1 4,12A8,8 0 0,1 12,4C12.76,4 13.5,4.11 14.2, 4.31L15.77,2.74C14.61,2.26 13.34,2 12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0, 0 22,12M7.91,10.08L6.5,11.5L11,16L21,6L19.59,4.58L11,13.17L7.91,10.08Z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const SuccessOutlinedIcon = createSvgIcon(() => _tmpl$$h(), "SuccessOutlined");

function getAlertUtilityClass(slot) {
  return generateUtilityClass("MuiAlert", slot);
}
const alertClasses = generateUtilityClasses("MuiAlert", [
  "root",
  "action",
  "icon",
  "message",
  "filled",
  "filledSuccess",
  "filledInfo",
  "filledWarning",
  "filledError",
  "outlined",
  "outlinedSuccess",
  "outlinedInfo",
  "outlinedWarning",
  "outlinedError",
  "standard",
  "standardSuccess",
  "standardInfo",
  "standardWarning",
  "standardError",
]);

const $$1p = createComponentFactory()({
  name: "MuiAlert",
  selfPropNames: [
    "action",
    "classes",
    "closeText",
    "color",
    "icon",
    "iconMapping",
    "onClose",
    "role",
    "severity",
    "variant",
  ],
  utilityClass: getAlertUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      `${ownerState.variant}${capitalize(
        ownerState.color || ownerState.severity,
      )}`,
      `${ownerState.variant}`,
    ],
    icon: ["icon"],
    message: ["message"],
    action: ["action"],
  }),
  propDefaults: ({ set }) =>
    set({
      closeText: "Close",
      //iconMapping = defaultIconMapping,
      role: "alert",
      severity: "success",
      variant: "standard",
    }),
});
const AlertRoot = styled$1(Paper, {
  name: "MuiAlert",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[ownerState.variant],
      styles[
        `${ownerState.variant}${capitalize(
          ownerState.color || ownerState.severity,
        )}`
      ],
    ];
  },
})(({ theme, ownerState }) => {
  const getColor = theme.palette.mode === "light" ? darken : lighten;
  const getBackgroundColor = theme.palette.mode === "light" ? lighten : darken;
  const color = ownerState.color || ownerState.severity;
  return {
    ...theme.typography.body2,
    backgroundColor: "transparent",
    display: "flex",
    padding: "6px 16px",
    ...(color &&
      ownerState.variant === "standard" && {
        color: getColor(theme.palette[color].light, 0.6),
        backgroundColor: getBackgroundColor(theme.palette[color].light, 0.9),
        [`& .${alertClasses.icon}`]: {
          color:
            theme.palette.mode === "dark"
              ? theme.palette[color].main
              : theme.palette[color].light,
        },
      }),
    ...(color &&
      ownerState.variant === "outlined" && {
        color: getColor(theme.palette[color].light, 0.6),
        border: `1px solid ${theme.palette[color].light}`,
        [`& .${alertClasses.icon}`]: {
          color:
            theme.palette.mode === "dark"
              ? theme.palette[color].main
              : theme.palette[color].light,
        },
      }),
    ...(color &&
      ownerState.variant === "filled" && {
        color: "#fff",
        fontWeight: theme.typography.fontWeightMedium,
        backgroundColor:
          theme.palette.mode === "dark"
            ? theme.palette[color].dark
            : theme.palette[color].main,
      }),
  };
});
const AlertIcon = styled$1("div", {
  name: "MuiAlert",
  slot: "Icon",
  overridesResolver: (props, styles) => styles.icon,
})({
  marginRight: 12,
  padding: "7px 0",
  display: "flex",
  fontSize: 22,
  opacity: 0.9,
});
const AlertMessage = styled$1("div", {
  name: "MuiAlert",
  slot: "Message",
  overridesResolver: (props, styles) => styles.message,
})({
  padding: "8px 0",
});
const AlertAction = styled$1("div", {
  name: "MuiAlert",
  slot: "Action",
  overridesResolver: (props, styles) => styles.action,
})({
  display: "flex",
  alignItems: "flex-start",
  padding: "4px 0 0 16px",
  marginLeft: "auto",
  marginRight: -8,
});
const defaultIconMapping = {
  success: () =>
    createComponent(SuccessOutlinedIcon, {
      fontSize: "inherit",
    }),
  warning: () =>
    createComponent(ReportProblemOutlinedIcon, {
      fontSize: "inherit",
    }),
  error: () =>
    createComponent(ErrorOutlineIcon, {
      fontSize: "inherit",
    }),
  info: () =>
    createComponent(InfoOutlinedIcon, {
      fontSize: "inherit",
    }),
};

/**
 *
 * Demos:
 *
 * - [Alert](https://mui.com/components/alert/)
 *
 * API:
 *
 * - [Alert API](https://mui.com/api/alert/)
 * - inherits [Paper API](https://mui.com/api/paper/)
 */

const Alert = $$1p.component(function Alert({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const icon = createMemo(() => props.icon);
  const action = createMemo(() => props.action);
  return createComponent(
    AlertRoot,
    mergeProps(
      {
        get role() {
          return props.role;
        },
        elevation: 0,
        ownerState: allProps,
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
      },
      otherProps,
      {
        get children() {
          return [
            createComponent(Show, {
              get when() {
                return icon() !== false;
              },
              get children() {
                return createComponent(AlertIcon, {
                  ownerState: allProps,
                  get ["class"]() {
                    return classes.icon;
                  },
                  get children() {
                    return (
                      icon() ||
                      props.iconMapping?.[props.severity]?.() ||
                      defaultIconMapping[props.severity]?.()
                    );
                  },
                });
              },
            }),
            createComponent(AlertMessage, {
              ownerState: allProps,
              get ["class"]() {
                return classes.message;
              },
              get children() {
                return otherProps.children;
              },
            }),
            createComponent(Show, {
              get when() {
                return action();
              },
              get children() {
                return createComponent(AlertAction, {
                  get ["class"]() {
                    return classes.action;
                  },
                  get children() {
                    return action();
                  },
                });
              },
            }),
            createComponent(Show, {
              get when() {
                return !action() && props.onClose;
              },
              get children() {
                return createComponent(AlertAction, {
                  ownerState: allProps,
                  get ["class"]() {
                    return classes.action;
                  },
                  get children() {
                    return createComponent(IconButton, {
                      size: "small",
                      get ["aria-label"]() {
                        return props.closeText;
                      },
                      get title() {
                        return props.closeText;
                      },
                      color: "inherit",
                      get onClick() {
                        return props.onClose;
                      },
                      get children() {
                        return createComponent(CloseIcon, {
                          fontSize: "small",
                        });
                      },
                    });
                  },
                });
              },
            }),
          ];
        },
      },
    ),
  );
});

function getTypographyUtilityClass(slot) {
  return generateUtilityClass("MuiTypography", slot);
}
generateUtilityClasses("MuiTypography", [
  "root",
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",
  "subtitle1",
  "subtitle2",
  "body1",
  "body2",
  "inherit",
  "button",
  "caption",
  "overline",
  "alignLeft",
  "alignRight",
  "alignCenter",
  "alignJustify",
  "noWrap",
  "gutterBottom",
  "paragraph",
]);

const $$1o = createComponentFactory()({
  name: "MuiTypography",
  selfPropNames: [
    "align",
    "children",
    "classes",
    "gutterBottom",
    "noWrap",
    "paragraph",
    "variant",
    "variantMapping",
  ],
  utilityClass: getTypographyUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      ownerState.align !== "inherit" && `align${capitalize(ownerState.align)}`,
      ownerState.gutterBottom && "gutterBottom",
      ownerState.noWrap && "noWrap",
      ownerState.paragraph && "paragraph",
    ],
  }),
});
const TypographyRoot = styled$1("span", {
  name: "MuiTypography",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.variant && styles[ownerState.variant],
      ownerState.align !== "inherit" &&
        styles[`align${capitalize(ownerState.align)}`],
      ownerState.noWrap && styles.noWrap,
      ownerState.gutterBottom && styles.gutterBottom,
      ownerState.paragraph && styles.paragraph,
    ];
  },
})(({ theme, ownerState }) => ({
  margin: 0,
  color: ownerState.color,
  ...(ownerState.variant && theme.typography[ownerState.variant]),
  ...(ownerState.align !== "inherit" && {
    textAlign: ownerState.align,
  }),
  ...(ownerState.noWrap && {
    overflow: "hidden",
    textOverflow: "ellipsis",
    whiteSpace: "nowrap",
  }),
  ...(ownerState.gutterBottom && {
    marginBottom: "0.35em",
  }),
  ...(ownerState.paragraph && {
    marginBottom: 16,
  }),
}));
const defaultVariantMapping = {
  h1: "h1",
  h2: "h2",
  h3: "h3",
  h4: "h4",
  h5: "h5",
  h6: "h6",
  subtitle1: "h6",
  subtitle2: "h6",
  body1: "p",
  body2: "p",
  inherit: "p",
};

// TODO v6: deprecate these color values in v5.x and remove the transformation in v6
const colorTransformations$1 = {
  primary: "primary.main",
  textPrimary: "text.primary",
  secondary: "secondary.main",
  textSecondary: "text.secondary",
  error: "error.main",
};
const transformDeprecatedColors$1 = (color) => {
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  return colorTransformations$1[color] || color;
};

/**
 *
 * Demos:
 *
 * - [Breadcrumbs](https://mui.com/components/breadcrumbs/)
 * - [Typography](https://mui.com/components/typography/)
 *
 * API:
 *
 * - [Typography API](https://mui.com/api/typography/)
 */
const Typography = $$1o.defineComponent(function Typography(inProps) {
  const theme = useTheme$1();
  const themeProps = theme.components?.[$$1o.name]?.defaultProps;
  const color = () =>
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    transformDeprecatedColors$1(inProps.color ?? themeProps?.color);
  const [sx, props] = splitSxProps(
    mergeProps(
      {
        align: "inherit",
        variant: "body1",
        variantMapping: defaultVariantMapping,
      },
      themeProps,
      inProps,
      {
        get color() {
          return color();
        },
      },
    ),
  );
  const [, other] = splitProps(props, [
    "align",
    "class",
    "component",
    "gutterBottom",
    "noWrap",
    "paragraph",
    "variant",
    "variantMapping",
  ]);
  const ownerState = {
    get classes() {
      return props.classes;
    },
    get align() {
      return props.align;
    },
    get color() {
      return color();
    },
    get gutterBottom() {
      return props.gutterBottom || false;
    },
    get noWrap() {
      return props.noWrap || false;
    },
    get paragraph() {
      return props.paragraph || false;
    },
    get variant() {
      return props.variant;
    },
  };
  const Component = () =>
    props.component ||
    (props.paragraph
      ? "p"
      : props.variantMapping[props.variant] ||
        defaultVariantMapping[props.variant]) ||
    "span";
  const classes = $$1o.useClasses(ownerState);
  return createComponent(
    TypographyRoot,
    mergeProps(
      {
        get as() {
          return Component();
        },
        get sx() {
          return sx();
        },
        ownerState: ownerState,
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
      },
      other,
    ),
  );
});

function getAlertTitleUtilityClass(slot) {
  return generateUtilityClass("MuiAlertTitle", slot);
}
generateUtilityClasses("MuiAlertTitle", ["root"]);

const $$1n = createComponentFactory()({
  name: "MuiAlertTitle",
  selfPropNames: ["children", "classes"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
    }),
  utilityClass: getAlertTitleUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const AlertTitleRoot = styled$1(Typography, {
  name: "MuiAlertTitle",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})(({ theme }) => {
  return {
    fontWeight: theme.typography.fontWeightMedium,
    marginTop: -2,
  };
});

/**
 *
 * Demos:
 *
 * - [Alert](https://mui.com/components/alert/)
 *
 * API:
 *
 * - [AlertTitle API](https://mui.com/api/alert-title/)
 */
const AlertTitle = $$1n.component(function AlertTitle({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    AlertTitleRoot,
    mergeProps(
      {
        gutterBottom: true,
      },
      otherProps,
      {
        ownerState: allProps,
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        get children() {
          return props.children;
        },
      },
    ),
  );
});

function getAppBarUtilityClass(slot) {
  return generateUtilityClass("MuiAppBar", slot);
}
generateUtilityClasses("MuiAppBar", [
  "root",
  "positionFixed",
  "positionAbsolute",
  "positionSticky",
  "positionStatic",
  "positionRelative",
  "colorDefault",
  "colorPrimary",
  "colorSecondary",
  "colorInherit",
  "colorTransparent",
]);

const $$1m = createComponentFactory()({
  name: "MuiAppBar",
  selfPropNames: ["classes", "color", "enableColorOnDark", "position"],
  utilityClass: getAppBarUtilityClass,
  propDefaults: ({ set }) =>
    set({
      component: "header",
      color: "primary",
      enableColorOnDark: false,
      position: "fixed",
    }),
  slotClasses: (ownerState) => ({
    root: [
      "root",
      `color${capitalize(ownerState.color)}`,
      `position${capitalize(ownerState.position)}`,
    ],
  }),
});
const AppBarRoot = styled$1(Paper, {
  name: "MuiAppBar",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [
      styles.root,
      styles[`position${capitalize(props.ownerState.position)}`],
      styles[`color${capitalize(props.ownerState.color)}`],
    ];
  },
})(({ theme, ownerState }) => {
  const backgroundColorDefault =
    theme.palette.mode === "light"
      ? theme.palette.grey[100]
      : theme.palette.grey[900];
  return {
    display: "flex",
    flexDirection: "column",
    width: "100%",
    boxSizing: "border-box",
    // Prevent padding issue with the Modal and fixed positioned AppBar.
    flexShrink: 0,
    ...(ownerState.position === "fixed" && {
      position: "fixed",
      zIndex: theme.zIndex.appBar,
      top: 0,
      left: "auto",
      right: 0,
      "@media print": {
        // Prevent the app bar to be visible on each printed page.
        position: "absolute",
      },
    }),
    ...(ownerState.position === "absolute" && {
      position: "absolute",
      zIndex: theme.zIndex.appBar,
      top: 0,
      left: "auto",
      right: 0,
    }),
    ...(ownerState.position === "sticky" && {
      // ⚠️ sticky is not supported by IE11.
      position: "sticky",
      zIndex: theme.zIndex.appBar,
      top: 0,
      left: "auto",
      right: 0,
    }),
    ...(ownerState.position === "static" && {
      position: "static",
    }),
    ...(ownerState.position === "relative" && {
      position: "relative",
    }),
    ...(ownerState.color === "default" && {
      backgroundColor: backgroundColorDefault,
      color: theme.palette.getContrastText(backgroundColorDefault),
    }),
    ...(ownerState.color &&
      ownerState.color !== "default" &&
      ownerState.color !== "inherit" &&
      ownerState.color !== "transparent" && {
        backgroundColor: theme.palette[ownerState.color].main,
        color: theme.palette[ownerState.color].contrastText,
      }),
    ...(ownerState.color === "inherit" && {
      color: "inherit",
    }),
    ...(theme.palette.mode === "dark" &&
      !ownerState.enableColorOnDark && {
        backgroundColor: undefined,
        color: undefined,
      }),
    ...(ownerState.color === "transparent" && {
      backgroundColor: "transparent",
      color: "inherit",
      ...(theme.palette.mode === "dark" && {
        backgroundImage: "none",
      }),
    }),
  };
});

/**
 *
 * Demos:
 *
 * - [App Bar](https://mui.com/components/app-bar/)
 *
 * API:
 *
 * - [AppBar API](https://mui.com/api/app-bar/)
 * - inherits [Paper API](https://mui.com/api/paper/)
 */

const AppBar = $$1m.component(function AppBar({
  allProps,
  classes,
  props,
  otherProps,
}) {
  return createComponent(
    AppBarRoot,
    mergeProps(
      {
        square: true,
        ownerState: allProps,
        elevation: 4,
        get ["class"]() {
          return clsx(
            classes.root,
            {
              "mui-fixed": props.position === "fixed", // Useful for the Dialog
            },
            otherProps.class,
          );
        },
      },
      otherProps,
    ),
  );
});

const _tmpl$$g = /*#__PURE__*/ template(
  `<svg><path d="M12 12c2.21 0 4-1.79 4-4s-1.79-4-4-4-4 1.79-4 4 1.79 4 4 4zm0 2c-2.67 0-8 1.34-8 4v2h16v-2c0-2.66-5.33-4-8-4z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const Person = createSvgIcon(() => _tmpl$$g(), "Person");

function getAvatarUtilityClass(slot) {
  return generateUtilityClass("MuiAvatar", slot);
}
generateUtilityClasses("MuiAvatar", [
  "root",
  "colorDefault",
  "circular",
  "rounded",
  "square",
  "img",
  "fallback",
]);

const $$1l = createComponentFactory()({
  name: "MuiAvatar",
  selfPropNames: [
    "alt",
    "children",
    "classes",
    "imgProps",
    "sizes",
    "src",
    "srcSet",
    "variant",
  ],
  utilityClass: getAvatarUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      ownerState.colorDefault && "colorDefault",
    ],
    img: ["img"],
    fallback: ["fallback"],
  }),
});
const AvatarRoot = styled$1("div", {
  name: "MuiAvatar",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[ownerState.variant],
      ownerState.colorDefault && styles.colorDefault,
    ];
  },
})(({ theme, ownerState }) => ({
  position: "relative",
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  flexShrink: 0,
  width: 40,
  height: 40,
  fontFamily: theme.typography.fontFamily,
  fontSize: theme.typography.pxToRem(20),
  lineHeight: 1,
  borderRadius: "50%",
  overflow: "hidden",
  userSelect: "none",
  ...(ownerState.variant === "rounded" && {
    borderRadius: theme.shape.borderRadius,
  }),
  ...(ownerState.variant === "square" && {
    borderRadius: 0,
  }),
  ...(ownerState.colorDefault && {
    color: theme.palette.background.default,
    backgroundColor:
      theme.palette.mode === "light"
        ? theme.palette.grey[400]
        : theme.palette.grey[600],
  }),
}));
const AvatarImg = styled$1("img", {
  name: "MuiAvatar",
  slot: "Img",
  overridesResolver: (props, styles) => styles.img,
})({
  width: "100%",
  height: "100%",
  textAlign: "center",
  // Handle non-square image. The property isn't supported by IE11.
  objectFit: "cover",
  // Hide alt text.
  color: "transparent",
  // Hide the image broken icon, only works on Chrome.
  textIndent: "10000",
});
const AvatarFallback = styled$1(Person, {
  name: "MuiAvatar",
  slot: "Fallback",
  overridesResolver: (props, styles) => styles.fallback,
})({
  width: "75%",
  height: "75%",
});
function useLoaded(props) {
  let active = true;
  const [loaded, setLoaded] = createSignal(false);
  onCleanup(() => {
    active = false;
  });
  createEffect(
    on(
      () => [props.crossOrigin, props.referrerPolicy, props.src, props.srcSet],
      () => {
        if (!props.src && !props.srcSet) {
          return undefined;
        }
        setLoaded(false);
        const image = new Image();
        image.onload = () => {
          if (!active) {
            return;
          }
          setLoaded("loaded");
        };
        image.onerror = () => {
          if (!active) {
            return;
          }
          setLoaded("error");
        };
        image.crossOrigin = props.crossOrigin;
        image.referrerPolicy = props.referrerPolicy;
        image.src = props.src;
        if (props.srcSet) {
          image.srcset = props.srcSet;
        }
      },
    ),
  );
  return loaded;
}

/**
 *
 * Demos:
 *
 * - [Avatars](https://mui.com/components/avatars/)
 *
 * API:
 *
 * - [Avatar API](https://mui.com/api/avatar/)
 */
const Avatar = $$1l.defineComponent(function Avatar(inProps) {
  const props = $$1l.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "alt",
    "children",
    "class",
    "component",
    "imgProps",
    "sizes",
    "src",
    "srcSet",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      component: "div",
      variant: "circular",
    },
    props,
  );

  // Use a hook instead of onError on the img element to support server-side rendering.
  const loaded = useLoaded(
    mergeProps(() => props.imgProps || {}, {
      get src() {
        return props.src;
      },
      get srcSet() {
        return props.srcSet;
      },
    }),
  );
  const hasImg = () => props.src || props.srcSet;
  const hasImgNotFailing = () => hasImg() && loaded() !== "error";
  const ownerState = mergeProps(props, {
    get colorDefault() {
      return !hasImgNotFailing();
    },
    get component() {
      return baseProps.component;
    },
    get variant() {
      return baseProps.variant;
    },
  });
  const classes = $$1l.useClasses(ownerState);
  const children = () => {
    if (hasImgNotFailing()) {
      return createComponent(
        AvatarImg,
        mergeProps(
          {
            get alt() {
              return props.alt;
            },
            get src() {
              return props.src;
            },
            get srcSet() {
              return props.srcSet;
            },
            get sizes() {
              return props.sizes;
            },
            ownerState: ownerState,
            get ["class"]() {
              return classes.img;
            },
          },
          () => props.imgProps || {},
        ),
      );
    }
    const children = props.children;
    if (children !== null && children !== undefined) {
      return children;
    } else if (hasImg() && props.alt) {
      return props.alt[0];
    } else {
      return createComponent(AvatarFallback, {
        get ["class"]() {
          return classes.fallback;
        },
      });
    }
  };
  return createComponent(
    AvatarRoot,
    mergeProps(
      {
        get as() {
          return baseProps.component;
        },
        ownerState: ownerState,
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
      },
      other,
      {
        get children() {
          return children();
        },
      },
    ),
  );
});

const reflow = (node) => node.scrollTop;
function getTransitionProps(props, options) {
  const { timeout, easing, style = {} } = props;
  return {
    duration:
      style.transitionDuration ??
      (typeof timeout === "number"
        ? timeout
        : typeof timeout === "string"
        ? 0
        : timeout[options.mode] || 0),
    easing:
      style.transitionTimingFunction ??
      (typeof easing === "object" ? easing[options.mode] : easing),
    delay: style.transitionDelay,
  };
}

function resolveTransitionTimeout(timeout) {
  if (typeof timeout === "number") {
    return {
      exit: timeout,
      enter: timeout,
      appear: timeout,
    };
  } else {
    return {
      exit: timeout?.exit || 0,
      enter: timeout?.enter || 0,
      appear: timeout?.appear ?? timeout?.enter ?? 0,
    };
  }
}
function Transition(inProps) {
  const props = mergeProps(
    {
      in: false,
      mountOnEnter: false,
      unmountOnExit: false,
      appear: false,
      enter: true,
      exit: true,
    },
    inProps,
  );
  const timeouts = createMemo(() => resolveTransitionTimeout(props.timeout));
  let initialStatus;
  if (props.in) {
    if (props.appear) {
      initialStatus = "exited";
    } else {
      initialStatus = "entered";
    }
  } else {
    if (props.unmountOnExit || props.mountOnEnter) {
      initialStatus = "unmounted";
    } else {
      initialStatus = "exited";
    }
  }
  const [status, setStatus] = createSignal(initialStatus);
  let enteredTimeout;
  let exitedTimeout;
  let firstStatusChange = true;
  function onTransitionEnd(ms, cb) {
    const next = () => setTimeout(cb, ms);
    let timeout;
    let stopped = false;
    const stop = () => {
      stopped = true;
      timeout && clearTimeout(timeout);
    };
    if (props.addEndListener) {
      props.addEndListener(() => {
        if (!stopped) timeout = next();
      });
    } else {
      timeout = next();
    }
    return stop;
  }
  const result = createMemo(
    on(
      () => [status()],
      () => {
        const v = status();
        const result = v !== "unmounted" ? props.children(v) : undefined;
        if (firstStatusChange) {
          firstStatusChange = false;
          return result;
        }
        if (v === "entering") {
          props.onEntering?.();
          if (exitedTimeout) {
            exitedTimeout();
            exitedTimeout = undefined;
          }
          enteredTimeout = onTransitionEnd(timeouts().enter, () =>
            setStatus("entered"),
          );
        } else if (v === "entered") {
          props.onEntered?.();
        } else if (v === "exiting") {
          props.onExiting?.();
          if (enteredTimeout) {
            enteredTimeout();
            enteredTimeout = undefined;
          }
          exitedTimeout = onTransitionEnd(timeouts().exit, () =>
            setStatus("exited"),
          );
        } else if (v === "exited") {
          props.onExited?.();
        }
        return result;
      },
    ),
  );
  createEffect((firstTime) => {
    if (props.in) {
      untrack(() => props.onEnter?.());
      setStatus("entering");
    } else {
      if (!firstTime) {
        untrack(() => props.onExit?.());
        setStatus("exiting");
      }
    }
    return false;
  }, true);
  onCleanup(() => {
    enteredTimeout?.();
    exitedTimeout?.();
  });
  return result();
}

const TransitionContext = createContext();

const $$1k = createComponentFactory()({
  name: "MuiFader",
  selfPropNames: ["appear", "children", "easing", "in", "timeout"],
  propDefaults: ({ set }) => {
    const theme = useTheme$1();
    return set({
      appear: true,
      get timeout() {
        return {
          enter: theme.transitions.duration.enteringScreen,
          exit: theme.transitions.duration.leavingScreen,
        };
      },
    });
  },
});
const fadeSelfPropNames = $$1k.selfPropNames;

/**
 * The Fade transition is used by the [Modal](https://mui.com/components/modal/) component.
 * It uses [react-transition-group](https://github.com/reactjs/react-transition-group) internally.
 *
 * Demos:
 *
 * - [Transitions](https://mui.com/components/transitions/)
 *
 * API:
 *
 * - [Fade API](https://mui.com/api/fade/)
 * - inherits [Transition API](http://reactcommunity.org/react-transition-group/transition/#Transition-props)
 */
const Fade = $$1k.component(function Fade({ props, otherProps }) {
  const theme = useTheme$1();
  const element = createElementRef(props);
  const timeout = createMemo(() => resolveTransitionTimeout(props.timeout));
  const c = children(() => props.children);
  const context = useContext(TransitionContext);
  return createComponent(
    Transition,
    mergeProps(
      {
        get ["in"]() {
          return props.in ?? context?.in;
        },
        get appear() {
          return props.appear;
        },
        get timeout() {
          return props.timeout;
        },
      },
      otherProps,
      {
        ref: element,
        onEnter: () => {
          const e = c();
          reflow(e);
          const transitionProps = getTransitionProps(
            {
              style: otherProps.style,
              timeout: timeout(),
              easing: props.easing,
            },
            {
              mode: "enter",
            },
          );
          e.style.transition = theme.transitions.create(
            "opacity",
            transitionProps,
          );
          otherProps.onEnter?.();
          context?.onEnter?.();
        },
        onExit: () => {
          const e = c();
          const transitionProps = getTransitionProps(
            {
              style: otherProps.style,
              timeout: timeout(),
              easing: props.easing,
            },
            {
              mode: "enter",
            },
          );
          e.style.transition = theme.transitions.create(
            "opacity",
            transitionProps,
          );
          otherProps.onExit?.();
        },
        onExited: () => {
          otherProps.onExited?.();
          context?.onExited?.();
        },
        children: (state) => {
          const element = c();
          if (state === "exited" && !props.in) {
            element.style.visibility = "hidden";
          } else {
            element.style.removeProperty("visibility");
          }
          if (state === "entering" || state === "entered") {
            element.style.opacity = "1";
          } else {
            element.style.opacity = "0";
          }
          return element;
        },
      },
    ),
  );
});

/**
 * Determines if a given element is a DOM element name (i.e. not a React component).
 */
function isHostComponent(element) {
  return typeof element === "string";
}

function getBackdropUtilityClass(slot) {
  return generateUtilityClass("MuiBackdrop", slot);
}
generateUtilityClasses("MuiBackdrop", ["root", "invisible"]);

const $$1j = createComponentFactory()({
  name: "BackdropUnstyled",
  selfPropNames: [
    "children",
    "classes",
    "components",
    "componentsProps",
    "invisible",
  ],
  propDefaults: ({ set }) =>
    set({
      component: "div",
      components: {},
      componentsProps: {},
      invisible: false,
    }),
  utilityClass: getBackdropUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", ownerState.invisible && "invisible"],
  }),
});
/**
 *
 * Demos:
 *
 * - [Backdrop](https://mui.com/components/backdrop/)
 *
 * API:
 *
 * - [BackdropUnstyled API](https://mui.com/api/backdrop-unstyled/)
 */
const BackdropUnstyled = $$1j.component(function BackdropUnstyled({
  props,
  otherProps,
  allProps,
  classes,
}) {
  const Root = () => props.components.Root || otherProps.component;
  const rootProps = () => props.componentsProps.root || {};
  return createComponent(
    Dynamic,
    mergeProps(
      {
        "aria-hidden": true,
      },
      rootProps,
      otherProps,
      () =>
        !isHostComponent(Root()) && {
          ownerState: allProps,
        },
      {
        get $component() {
          return Root();
        },
        get ["class"]() {
          return clsx(classes.root, rootProps().class, otherProps.class);
        },
        get children() {
          return props.children;
        },
      },
    ),
  );
});

const $$1i = createComponentFactory()({
  name: "MuiBackdrop",
  selfPropNames: ["classes", "open", "transitionDuration"],
  propDefaults: ({ set }) =>
    set({
      open: false,
      component: "div",
    }),
});
const BackdropRoot = styled$1("div", {
  name: "MuiBackdrop",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.root, ownerState.invisible && styles.invisible];
  },
})(({ ownerState }) => ({
  position: "fixed",
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  right: 0,
  bottom: 0,
  top: 0,
  left: 0,
  backgroundColor: "rgba(0, 0, 0, 0.5)",
  WebkitTapHighlightColor: "transparent",
  ...(ownerState.invisible && {
    backgroundColor: "transparent",
  }),
}));

/**
 *
 * Demos:
 *
 * - [Backdrop](https://mui.com/components/backdrop/)
 *
 * API:
 *
 * - [Backdrop API](https://mui.com/api/backdrop/)
 * - inherits [Fade API](https://mui.com/api/fade/)
 */
const Backdrop = $$1i.component(function Backdrop({ props, otherProps }) {
  const [fadeProps, backdropProps] = splitProps(otherProps, fadeSelfPropNames);
  return createComponent(
    Fade,
    mergeProps(
      {
        get ["in"]() {
          return props.open;
        },
        get timeout() {
          return props.transitionDuration;
        },
      },
      fadeProps,
      {
        get children() {
          return createComponent(
            BackdropUnstyled,
            mergeProps(backdropProps, {
              get ["class"]() {
                return otherProps.class;
              },
              get invisible() {
                return otherProps.invisible;
              },
              get components() {
                return {
                  Root: BackdropRoot,
                  ...otherProps.components,
                };
              },
              get componentsProps() {
                return {
                  root: {
                    ...otherProps.componentsProps?.root,
                    ...((!otherProps.components?.Root ||
                      !isHostComponent(otherProps.components?.Root)) &&
                      {
                        //ownerState: { ...baseProps.componentsProps?.root?.ownerState },
                      }),
                  },
                };
              },
              get classes() {
                return props.classes;
              },
              get children() {
                return otherProps.children;
              },
            }),
          );
        },
      },
    ),
  );
});

/**
 * Appends the ownerState object to the props, merging with the existing one if necessary.
 *
 * @param elementType Type of the element that owns the `existingProps`. If the element is a DOM node, `ownerState` are not applied.
 * @param existingProps Props of the element.
 * @param ownerState
 */
function appendOwnerState(elementType, existingProps, ownerState) {
  const mergedOwnerState = mergeProps(
    () => existingProps().ownerState || {},
    ownerState,
  );
  return mergeProps(existingProps, {
    get ownerState() {
      if (!isHostComponent(elementType())) return mergedOwnerState;
    },
  });
}

function getBadgeUtilityClass(slot) {
  return generateUtilityClass("MuiBadge", slot);
}
const badgeUnstyledClasses = generateUtilityClasses("MuiBadge", [
  "root",
  "badge",
  "dot",
  "standard",
  "anchorOriginTopLeft",
  "anchorOriginTopRight",
  "anchorOriginBottomLeft",
  "anchorOriginBottomRight",
  "invisible",
]);

function useBadgeInvisibleMemo(props) {
  return createMemo(() => {
    if (
      props.invisible === false &&
      ((props.badgeContent === 0 && !props.showZero) ||
        (props.badgeContent == null && props.variant !== "dot"))
    ) {
      return true;
    } else {
      return props.invisible;
    }
  });
}
function useBadge(inProps) {
  const props = mergeProps(
    {
      anchorOrigin: {
        vertical: "top",
        horizontal: "right",
      },
      invisible: false,
      max: 99,
      showZero: false,
      variant: "standard",
    },
    inProps,
  );
  const prevProps = usePreviousProps(props, [
    "anchorOrigin",
    "badgeContent",
    "max",
    "variant",
  ]);
  const invisible = useBadgeInvisibleMemo(props);
  const badge = mergeProps(() => (invisible() ? prevProps : props));
  const displayValue = () => {
    if (badge.variant !== "dot") {
      return badge.badgeContent && Number(badge.badgeContent) > badge.max
        ? `${badge.max}+`
        : badge.badgeContent;
    }
  };
  return mergeProps(badge, {
    get invisible() {
      return invisible();
    },
    get displayValue() {
      return displayValue();
    },
  });
}

const $$1h = createComponentFactory()({
  name: "MuiBadgeUnstyled",
  autoCallUseClasses: false,
  selfPropNames: [
    "anchorOrigin",
    "badgeContent",
    "children",
    "classes",
    "components",
    "components",
    "componentsProps",
    "invisible",
    "max",
    "showZero",
    "variant",
  ],
  propDefaults: ({ set }) =>
    set({
      anchorOrigin: {
        vertical: "top",
        horizontal: "right",
      },
      components: {},
      componentsProps: {},
      max: 99,
      showZero: false,
      variant: "standard",
      invisible: false,
    }),
  utilityClass: getBadgeUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root"],
    badge: [
      "badge",
      ownerState.variant,
      `anchorOrigin${capitalize(ownerState.anchorOrigin.vertical)}${capitalize(
        ownerState.anchorOrigin.horizontal,
      )}`,
      ownerState.invisible && "invisible",
    ],
  }),
});
/**
 *
 * Demos:
 *
 * - [Badges](https://mui.com/components/badges/)
 *
 * API:
 *
 * - [BadgeUnstyled API](https://mui.com/api/badge-unstyled/)
 */
const BadgeUnstyled = $$1h.component(function BadgeUnstyled({
  allProps,
  otherProps,
  props,
}) {
  const badge = useBadge(allProps);
  const ownerState = mergeProps(allProps, badge);
  const Root = () => otherProps.component || props.components.Root || "span";
  const rootProps = appendOwnerState(
    Root,
    () => mergeProps(otherProps, props.componentsProps.root || {}),
    ownerState,
  );
  const Badge = () => props.components.Badge || "span";
  const badgeProps = appendOwnerState(
    Badge,
    () => props.componentsProps.badge || {},
    ownerState,
  );
  const classes = $$1h.useClasses(ownerState);
  return createComponent(
    Dynamic$1,
    mergeProps(rootProps, otherProps, {
      get component() {
        return Root();
      },
      get ["class"]() {
        return clsx(classes.root, rootProps.class, otherProps.class);
      },
      get children() {
        return [
          createMemo(() => props.children),
          createComponent(
            Dynamic$1,
            mergeProps(badgeProps, {
              get component() {
                return Badge();
              },
              get ["class"]() {
                return clsx(classes.badge, badgeProps.class);
              },
              get children() {
                return badge.displayValue;
              },
            }),
          ),
        ];
      },
    }),
  );
});

const badgeClasses = Object.assign(
  {},
  badgeUnstyledClasses,
  generateUtilityClasses("MuiBadge", [
    "colorError",
    "colorInfo",
    "colorPrimary",
    "colorSecondary",
    "colorSuccess",
    "colorWarning",
    "overlapRectangular",
    "overlapCircular",
    // TODO: v6 remove the overlap value from these class keys
    "anchorOriginTopLeftCircular",
    "anchorOriginTopLeftRectangular",
    "anchorOriginTopRightCircular",
    "anchorOriginTopRightRectangular",
    "anchorOriginBottomLeftCircular",
    "anchorOriginBottomLeftRectangular",
    "anchorOriginBottomRightCircular",
    "anchorOriginBottomRightRectangular",
  ]),
);

const $$1g = createComponentFactory()({
  name: "MuiBadge",
  selfPropNames: ["classes", "color", "overlap", "variant"],
  propDefaults: ({ set }) =>
    set({
      anchorOrigin: {
        vertical: "top",
        horizontal: "right",
      },
      component: "span",
      components: {},
      componentsProps: {},
      overlap: "rectangular",
      color: "default",
      invisible: false,
      showZero: false,
      variant: "standard",
      max: 99,
    }),
  autoCallUseClasses: false,
  utilityClass: getBadgeUtilityClass,
  // [review]
  slotClasses: (ownerState) => {
    return {
      badge: [
        `anchorOrigin${capitalize(
          ownerState.anchorOrigin.vertical,
        )}${capitalize(ownerState.anchorOrigin.horizontal)}${capitalize(
          ownerState.overlap,
        )}`,
        `overlap${capitalize(ownerState.overlap)}`,
        ownerState.color !== "default" &&
          `color${capitalize(ownerState.color)}`,
      ],
    };
  },
});
const RADIUS_STANDARD = 10;
const RADIUS_DOT = 4;
const shouldSpreadAdditionalProps = (Slot) => {
  return !Slot || !isHostComponent(Slot);
};
const BadgeRoot = styled$1("span", {
  name: "MuiBadge",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  position: "relative",
  display: "inline-flex",
  // For correct alignment with the text.
  verticalAlign: "middle",
  flexShrink: 0,
});
const BadgeBadge = styled$1("span", {
  name: "MuiBadge",
  slot: "Badge",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.badge,
      styles[ownerState.variant],
      styles[
        `anchorOrigin${capitalize(
          ownerState.anchorOrigin.vertical,
        )}${capitalize(ownerState.anchorOrigin.horizontal)}${capitalize(
          ownerState.overlap,
        )}`
      ],
      ownerState.color !== "default" &&
        styles[`color${capitalize(ownerState.color)}`],
      ownerState.invisible && styles.invisible,
    ];
  },
})(({ theme, ownerState }) => ({
  display: "flex",
  flexDirection: "row",
  flexWrap: "wrap",
  justifyContent: "center",
  alignContent: "center",
  alignItems: "center",
  position: "absolute",
  boxSizing: "border-box",
  fontFamily: theme.typography.fontFamily,
  fontWeight: theme.typography.fontWeightMedium,
  fontSize: theme.typography.pxToRem(12),
  minWidth: RADIUS_STANDARD * 2,
  lineHeight: 1,
  padding: "0 6px",
  height: RADIUS_STANDARD * 2,
  borderRadius: RADIUS_STANDARD,
  zIndex: 1,
  // Render the badge on top of potential ripples.
  transition: theme.transitions.create("transform", {
    easing: theme.transitions.easing.easeInOut,
    duration: theme.transitions.duration.enteringScreen,
  }),
  ...(ownerState.color !== "default" && {
    backgroundColor: theme.palette[ownerState.color].main,
    color: theme.palette[ownerState.color].contrastText,
  }),
  ...(ownerState.variant === "dot" && {
    borderRadius: RADIUS_DOT,
    height: RADIUS_DOT * 2,
    minWidth: RADIUS_DOT * 2,
    padding: 0,
  }),
  ...(ownerState.anchorOrigin.vertical === "top" &&
    ownerState.anchorOrigin.horizontal === "right" &&
    ownerState.overlap === "rectangular" && {
      top: 0,
      right: 0,
      transform: "scale(1) translate(50%, -50%)",
      transformOrigin: "100% 0%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(50%, -50%)",
      },
    }),
  ...(ownerState.anchorOrigin.vertical === "bottom" &&
    ownerState.anchorOrigin.horizontal === "right" &&
    ownerState.overlap === "rectangular" && {
      bottom: 0,
      right: 0,
      transform: "scale(1) translate(50%, 50%)",
      transformOrigin: "100% 100%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(50%, 50%)",
      },
    }),
  ...(ownerState.anchorOrigin.vertical === "top" &&
    ownerState.anchorOrigin.horizontal === "left" &&
    ownerState.overlap === "rectangular" && {
      top: 0,
      left: 0,
      transform: "scale(1) translate(-50%, -50%)",
      transformOrigin: "0% 0%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(-50%, -50%)",
      },
    }),
  ...(ownerState.anchorOrigin.vertical === "bottom" &&
    ownerState.anchorOrigin.horizontal === "left" &&
    ownerState.overlap === "rectangular" && {
      bottom: 0,
      left: 0,
      transform: "scale(1) translate(-50%, 50%)",
      transformOrigin: "0% 100%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(-50%, 50%)",
      },
    }),
  ...(ownerState.anchorOrigin.vertical === "top" &&
    ownerState.anchorOrigin.horizontal === "right" &&
    ownerState.overlap === "circular" && {
      top: "14%",
      right: "14%",
      transform: "scale(1) translate(50%, -50%)",
      transformOrigin: "100% 0%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(50%, -50%)",
      },
    }),
  ...(ownerState.anchorOrigin.vertical === "bottom" &&
    ownerState.anchorOrigin.horizontal === "right" &&
    ownerState.overlap === "circular" && {
      bottom: "14%",
      right: "14%",
      transform: "scale(1) translate(50%, 50%)",
      transformOrigin: "100% 100%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(50%, 50%)",
      },
    }),
  ...(ownerState.anchorOrigin.vertical === "top" &&
    ownerState.anchorOrigin.horizontal === "left" &&
    ownerState.overlap === "circular" && {
      top: "14%",
      left: "14%",
      transform: "scale(1) translate(-50%, -50%)",
      transformOrigin: "0% 0%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(-50%, -50%)",
      },
    }),
  ...(ownerState.anchorOrigin.vertical === "bottom" &&
    ownerState.anchorOrigin.horizontal === "left" &&
    ownerState.overlap === "circular" && {
      bottom: "14%",
      left: "14%",
      transform: "scale(1) translate(-50%, 50%)",
      transformOrigin: "0% 100%",
      [`&.${badgeClasses.invisible}`]: {
        transform: "scale(0) translate(-50%, 50%)",
      },
    }),
  ...(ownerState.invisible && {
    transition: theme.transitions.create("transform", {
      easing: theme.transitions.easing.easeInOut,
      duration: theme.transitions.duration.leavingScreen,
    }),
  }),
}));

/**
 *
 * Demos:
 *
 * - [Avatars](https://mui.com/components/avatars/)
 * - [Badges](https://mui.com/components/badges/)
 *
 * API:
 *
 * - [Badge API](https://mui.com/api/badge/)
 * - inherits [BadgeUnstyled API](https://mui.com/api/badge-unstyled/)
 */
const Badge = $$1g.component(function Badge({ allProps, otherProps, props }) {
  const [, otherPropsWithotuComponent] = splitProps(otherProps, ["component"]);
  const prevProps = usePreviousProps(allProps, [
    "anchorOrigin",
    "color",
    "overlap",
  ]);
  const invisible = useBadgeInvisibleMemo(allProps);
  const badge = mergeProps(() => (invisible() ? prevProps : allProps));
  const ownerState = mergeProps(allProps, {
    get invisible() {
      return invisible();
    },
  });
  const classes = $$1g.useClasses(ownerState);
  const components = mergeProps(
    {
      Root: BadgeRoot,
      Badge: BadgeBadge,
    },
    () => otherProps.components,
  );
  const rootOwnerState = mergeProps(
    () => otherProps.componentsProps.root?.["ownerState"] || {},
    {
      get color() {
        return badge.color;
      },
      get overlap() {
        return badge.overlap;
      },
    },
  );
  const badgeOwnerState = mergeProps(
    () => otherProps.componentsProps.badge?.["ownerState"] || {},
    {
      get color() {
        return badge.color;
      },
      get overlap() {
        return badge.overlap;
      },
    },
  );
  const componentProps = {
    root: mergeProps(
      () => otherProps.componentsProps.root || {},
      () =>
        shouldSpreadAdditionalProps(otherProps.components.Root) && {
          as: otherProps.component,
          ownerState: rootOwnerState,
        },
    ),
    badge: mergeProps(
      () => otherProps.componentsProps.badge || {},
      () =>
        shouldSpreadAdditionalProps(otherProps.components.Badge) && {
          ownerState: badgeOwnerState,
        },
    ),
  };
  return createComponent(
    BadgeUnstyled,
    mergeProps(
      {
        get variant() {
          return props.variant;
        },
      },
      otherPropsWithotuComponent,
      {
        components: components,
        componentsProps: componentProps,
        classes: classes,
      },
    ),
  );
});

const BottomNavigationContext = createContext({
  onChange: () => () => void 0,
  showLabels: () => false,
  selectedValue: () => void 0,
  getIndex: () => 0,
});

function getBottomNavigationUtilityClass(slot) {
  return generateUtilityClass("MuiBottomNavigation", slot);
}
generateUtilityClasses("MuiBottomNavigation", ["root"]);

const $$1f = createComponentFactory()({
  name: "MuiBottomNavigation",
  selfPropNames: ["children", "classes", "onChange", "showLabels", "value"],
  utilityClass: getBottomNavigationUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const BottomNavigationRoot = styled$1("div", {
  name: "MuiBottomNavigation",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})(({ theme }) => ({
  display: "flex",
  justifyContent: "center",
  height: 56,
  backgroundColor: theme.palette.background.paper,
}));

/**
 *
 * Demos:
 *
 * - [Bottom Navigation](https://mui.com/components/bottom-navigation/)
 *
 * API:
 *
 * - [BottomNavigation API](https://mui.com/api/bottom-navigation/)
 */
const BottomNavigation = $$1f.defineComponent(
  function BottomNavigation(inProps) {
    const ref = createRef(inProps);
    const props = $$1f.useThemeProps({
      props: inProps,
    });
    const [, other] = splitProps(props, [
      "children",
      "class",
      "component",
      "onChange",
      "showLabels",
      "value",
    ]);
    const baseProps = mergeProps(
      {
        component: "div",
        showLabels: false,
      },
      props,
    );
    const ownerState = mergeProps(props, {
      get component() {
        return baseProps.component;
      },
      get showLabels() {
        return baseProps.showLabels;
      },
    });
    const classes = $$1f.useClasses(ownerState);
    let resolvedChildren;
    return createComponent(
      BottomNavigationRoot,
      mergeProps(
        {
          get as() {
            return baseProps.component;
          },
          get ["class"]() {
            return clsx(classes.root, props.class);
          },
          ref: ref,
          ownerState: ownerState,
        },
        other,
        {
          get children() {
            return createComponent(BottomNavigationContext.Provider, {
              get value() {
                return {
                  showLabels: createMemo(() => props.showLabels),
                  selectedValue: createMemo(() => props.value),
                  onChange: createMemo(() => props.onChange),
                  getIndex: (child) => {
                    return resolvedChildren.toArray().indexOf(child);
                  },
                };
              },
              get children() {
                return (resolvedChildren = children(() => props.children))();
              },
            });
          },
        },
      ),
    );
  },
);

function getBottomNavigationActionUtilityClass(slot) {
  return generateUtilityClass("MuiBottomNavigationAction", slot);
}
const bottomNavigationActionClasses = generateUtilityClasses(
  "MuiBottomNavigationAction",
  ["root", "iconOnly", "selected", "label"],
);

const $$1e = createComponentFactory()({
  name: "MuiBottomNavigationAction",
  selfPropNames: ["children", "classes", "icon", "label", "showLabel", "value"],
  utilityClass: getBottomNavigationActionUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !ownerState.showLabel && !ownerState.selected && "iconOnly",
      ownerState.selected && "selected",
    ],
    label: [
      "label",
      !ownerState.showLabel && !ownerState.selected && "iconOnly",
      ownerState.selected && "selected",
    ],
  }),
});
const BottomNavigationActionRoot = styled$1(ButtonBase, {
  name: "MuiBottomNavigationAction",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      !ownerState.showLabel && !ownerState.selected && styles.iconOnly,
    ];
  },
})(({ theme, ownerState }) => ({
  transition: theme.transitions.create(["color", "padding-top"], {
    duration: theme.transitions.duration.short,
  }),
  padding: "6px 12px 8px",
  minWidth: 80,
  maxWidth: 168,
  color: theme.palette.text.secondary,
  flexDirection: "column",
  flex: "1",
  ...(!ownerState.showLabel &&
    !ownerState.selected && {
      paddingTop: 16,
    }),
  [`&.${bottomNavigationActionClasses.selected}`]: {
    paddingTop: 6,
    color: theme.palette.primary.main,
  },
}));
const BottomNavigationActionLabel = styled$1("span", {
  name: "MuiBottomNavigationAction",
  slot: "Label",
  overridesResolver: (props, styles) => styles.label,
})(({ theme, ownerState }) => ({
  fontFamily: theme.typography.fontFamily,
  fontSize: theme.typography.pxToRem(12),
  opacity: 1,
  transition: "font-size 0.2s, opacity 0.2s",
  transitionDelay: "0.1s",
  ...(!ownerState.showLabel &&
    !ownerState.selected && {
      opacity: 0,
      transitionDelay: "0s",
    }),
  [`&.${bottomNavigationActionClasses.selected}`]: {
    fontSize: theme.typography.pxToRem(14),
  },
}));

/**
 *
 * Demos:
 *
 * - [Bottom Navigation](https://mui.com/components/bottom-navigation/)
 *
 * API:
 *
 * - [BottomNavigationAction API](https://mui.com/api/bottom-navigation-action/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */
const BottomNavigationAction = $$1e.defineComponent(
  function BottomNavigationAction(inProps) {
    const ref = createRef(inProps);
    const props = $$1e.useThemeProps({
      props: inProps,
    });
    const [, other] = splitProps(props, [
      "class",
      "icon",
      "label",
      "onClick",
      "showLabel",
      "value",
    ]);
    const navCtx = useContext(BottomNavigationContext);
    const [self, setSelf] = createSignal();
    onMount(() => setSelf(ref.current));
    const value = createMemo(() => {
      if (props.value !== undefined) {
        return props.value;
      }
      if (self() !== undefined) {
        return navCtx.getIndex(self());
      }
      return undefined;
    });
    const handleChange = (event) => {
      const onChange = navCtx.onChange();
      if (typeof onChange === "function") {
        onChange(event, value());
      }
      if (typeof props.onClick === "function") {
        props.onClick(event);
      }
    };
    const ownerState = mergeProps(props, {
      get selected() {
        return navCtx.selectedValue() === value();
      },
      get showLabel() {
        return props.showLabel ?? navCtx.showLabels();
      },
    });
    const classes = $$1e.useClasses(ownerState);
    return createComponent(
      BottomNavigationActionRoot,
      mergeProps(
        {
          ref: ref,
          get ["class"]() {
            return clsx(classes.root, props.class);
          },
          focusRipple: true,
          onClick: handleChange,
          ownerState: ownerState,
        },
        other,
        {
          get children() {
            return [
              createMemo(() => props.icon),
              createComponent(BottomNavigationActionLabel, {
                get ["class"]() {
                  return classes.label;
                },
                ownerState: ownerState,
                get children() {
                  return props.label;
                },
              }),
            ];
          },
        },
      ),
    );
  },
);

const _tmpl$$f = /*#__PURE__*/ template(
  `<svg><path d="M6 10c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm12 0c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm-6 0c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const MoreHorizIcon = createSvgIcon(() => _tmpl$$f(), "MoreHoriz");

const _tmpl$$e = /*#__PURE__*/ template(`<li>`);
const BreadcrumbCollapsedButton = styled$1(ButtonBase)(({ theme }) => ({
  display: "flex",
  marginLeft: `calc(${theme.spacing(1)} * 0.5)`,
  marginRight: `calc(${theme.spacing(1)} * 0.5)`,
  ...(theme.palette.mode === "light"
    ? {
        backgroundColor: theme.palette.grey[100],
        color: theme.palette.grey[700],
      }
    : {
        backgroundColor: theme.palette.grey[700],
        color: theme.palette.grey[100],
      }),
  borderRadius: 2,
  "&:hover, &:focus": {
    ...(theme.palette.mode === "light"
      ? {
          backgroundColor: theme.palette.grey[200],
        }
      : {
          backgroundColor: theme.palette.grey[600],
        }),
  },
  "&:active": {
    boxShadow: theme.shadows[0],
    ...(theme.palette.mode === "light"
      ? {
          backgroundColor: emphasize(theme.palette.grey[200], 0.12),
        }
      : {
          backgroundColor: emphasize(theme.palette.grey[600], 0.12),
        }),
  },
}));
const BreadcrumbCollapsedIcon = styled$1(MoreHorizIcon)({
  width: 24,
  height: 16,
});

/**
 * @ignore - internal component.
 */
function BreadcrumbCollapsed(props) {
  return (() => {
    const _el$ = _tmpl$$e();
    insert(
      _el$,
      createComponent(
        BreadcrumbCollapsedButton,
        mergeProps(
          {
            focusRipple: true,
          },
          props,
          {
            ownerState: props,
            get children() {
              return createComponent(BreadcrumbCollapsedIcon, {
                ownerState: props,
              });
            },
          },
        ),
      ),
    );
    return _el$;
  })();
}

function getBreadcrumbsUtilityClass(slot) {
  return generateUtilityClass("MuiBreadcrumbs", slot);
}
const breadcrumbsClasses = generateUtilityClasses("MuiBreadcrumbs", [
  "root",
  "ol",
  "li",
  "separator",
]);

const _tmpl$$d = /*#__PURE__*/ template(`<li>`);
const $$1d = createComponentFactory()({
  name: "MuiBreadcrumbs",
  propDefaults: ({ set }) =>
    set({
      component: "nav",
      expandText: "Show path",
      itemsAfterCollapse: 1,
      itemsBeforeCollapse: 1,
      maxItems: 8,
      separator: "/",
    }),
  selfPropNames: [
    "children",
    "classes",
    "expandText",
    "itemsAfterCollapse",
    "itemsBeforeCollapse",
    "maxItems",
    "separator",
  ],
  utilityClass: getBreadcrumbsUtilityClass,
  slotClasses: () => ({
    root: ["root"],
    li: ["li"],
    ol: ["ol"],
    separator: ["separator"],
  }),
});
const BreadcrumbsRoot = styled$1(Typography, {
  name: "MuiBreadcrumbs",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [
      {
        [`& .${breadcrumbsClasses.li}`]: styles.li,
      },
      styles.root,
    ];
  },
})({});
const BreadcrumbsOl = styled$1("ol", {
  name: "MuiBreadcrumbs",
  slot: "Ol",
  overridesResolver: (props, styles) => styles.ol,
})({
  display: "flex",
  flexWrap: "wrap",
  alignItems: "center",
  padding: 0,
  margin: 0,
  listStyle: "none",
});
const BreadcrumbsSeparator = styled$1("li", {
  name: "MuiBreadcrumbs",
  slot: "Separator",
  overridesResolver: (props, styles) => styles.separator,
})({
  display: "flex",
  userSelect: "none",
  marginLeft: 8,
  marginRight: 8,
});
function insertSeparators(items, className, separator, ownerState) {
  return items.reduce((acc, current, index) => {
    if (index < items.length - 1) {
      acc = acc.concat(
        current,
        createComponent(BreadcrumbsSeparator, {
          as: "div",
          "aria-hidden": true,
          class: className,
          ownerState: ownerState,
          children: separator,
        }),
      );
    } else {
      acc.push(current);
    }
    return acc;
  }, []);
}

/**
 *
 * Demos:
 *
 * - [Breadcrumbs](https://mui.com/components/breadcrumbs/)
 *
 * API:
 *
 * - [Breadcrumbs API](https://mui.com/api/breadcrumbs/)
 */
const Breadcrumbs = $$1d.component(function Breadcrumbs({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const [expanded, setExpanded] = createSignal(false);
  const listElement = createElementRef();
  const ownerState = mergeProps(allProps, {
    get expanded() {
      return expanded();
    },
  });
  const handleClickExpand = () => {
    setExpanded(true);

    // The clicked element received the focus but gets removed from the DOM.
    // Let's keep the focus in the component after expanding.
    // Moving it to the <ol> or <nav> does not cause any announcement in NVDA.
    // By moving it to some link/button at least we have some announcement.
    const focusable = listElement.ref.querySelector(
      "a[href],button,[tabindex]",
    );
    if (focusable) {
      focusable.focus();
    }
  };
  const renderItemsBeforeAndAfter = (allItems) => {
    // This defends against someone passing weird input, to ensure that if all
    // items would be shown anyway, we just show all items without the EllipsisItem
    if (
      props.itemsBeforeCollapse + props.itemsAfterCollapse >=
      allItems.length
    ) {
      return allItems;
    }
    return [
      ...allItems.slice(0, props.itemsBeforeCollapse),
      createComponent(BreadcrumbCollapsed, {
        get ["aria-label"]() {
          return props.expandText;
        },
        onClick: handleClickExpand,
      }),
      ...allItems.slice(
        allItems.length - props.itemsAfterCollapse,
        allItems.length,
      ),
    ];
  };
  const resolved = children(() => props.children);
  const allItems = createMemo(() => {
    const value = resolved();
    const array = Array.isArray(value) ? value : [value];
    return array
      .filter((item) => (item ?? false) !== false)
      .map((item) =>
        (() => {
          const _el$ = _tmpl$$d();
          insert(_el$, item);
          createRenderEffect(() => className(_el$, classes.li));
          return _el$;
        })(),
      );
  });
  return createComponent(
    BreadcrumbsRoot,
    mergeProps(
      {
        color: "text.secondary",
      },
      otherProps,
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        ownerState: ownerState,
        get children() {
          return createComponent(BreadcrumbsOl, {
            get ["class"]() {
              return classes.ol;
            },
            ownerState: ownerState,
            ref: listElement,
            get children() {
              return insertSeparators(
                expanded() ||
                  (props.maxItems && allItems().length <= props.maxItems)
                  ? allItems()
                  : renderItemsBeforeAndAfter(allItems()),
                classes.separator,
                props.separator,
                ownerState,
              );
            },
          });
        },
      },
    ),
  );
});

const ButtonGroupContext = createContext({});

function getButtonUtilityClass(slot) {
  return generateUtilityClass("MuiButton", slot);
}
const buttonClasses = generateUtilityClasses("MuiButton", [
  "root",
  "text",
  "textInherit",
  "textPrimary",
  "textSecondary",
  "outlined",
  "outlinedInherit",
  "outlinedPrimary",
  "outlinedSecondary",
  "contained",
  "containedInherit",
  "containedPrimary",
  "containedSecondary",
  "disableElevation",
  "focusVisible",
  "disabled",
  "colorInherit",
  "textSizeSmall",
  "textSizeMedium",
  "textSizeLarge",
  "outlinedSizeSmall",
  "outlinedSizeMedium",
  "outlinedSizeLarge",
  "containedSizeSmall",
  "containedSizeMedium",
  "containedSizeLarge",
  "sizeMedium",
  "sizeSmall",
  "sizeLarge",
  "fullWidth",
  "startIcon",
  "endIcon",
  "iconSizeSmall",
  "iconSizeMedium",
  "iconSizeLarge",
]);

const $$1c = createComponentFactory()({
  name: "MuiButton",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disableElevation",
    "disableFocusRipple",
    "disabled",
    "endIcon",
    "fullWidth",
    "href",
    "size",
    "startIcon",
    "variant",
  ],
  propDefaults: ({ set }) => {
    const contextProps = useContext(ButtonGroupContext);
    return set({
      get color() {
        return contextProps.color ?? "primary";
      },
      component: "button",
      get disabled() {
        return contextProps.disabled ?? false;
      },
      get disableElevation() {
        return contextProps.disableElevation ?? false;
      },
      get disableFocusRipple() {
        return contextProps.disableFocusRipple ?? false;
      },
      get fullWidth() {
        return contextProps.fullWidth ?? false;
      },
      get size() {
        return contextProps.size ?? "medium";
      },
      get variant() {
        return contextProps.variant ?? "text";
      },
    });
  },
  utilityClass: getButtonUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      `${ownerState.variant}${capitalize(ownerState.color)}`,
      `size${capitalize(ownerState.size)}`,
      `${ownerState.variant}Size${capitalize(ownerState.size)}`,
      ownerState.color === "inherit" && "colorInherit",
      ownerState.disableElevation && "disableElevation",
      ownerState.fullWidth && "fullWidth",
    ],
    label: ["label"],
    startIcon: ["startIcon", `iconSize${capitalize(ownerState.size)}`],
    endIcon: ["endIcon", `iconSize${capitalize(ownerState.size)}`],
  }),
});
const commonIconStyles = (ownerState) => ({
  ...(ownerState.size === "small" && {
    "& > *:nth-of-type(1)": {
      fontSize: 18,
    },
  }),
  ...(ownerState.size === "medium" && {
    "& > *:nth-of-type(1)": {
      fontSize: 20,
    },
  }),
  ...(ownerState.size === "large" && {
    "& > *:nth-of-type(1)": {
      fontSize: 22,
    },
  }),
});
const ButtonRoot = styled$1(ButtonBase, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiButton",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[ownerState.variant],
      styles[`${ownerState.variant}${capitalize(ownerState.color)}`],
      styles[`size${capitalize(ownerState.size)}`],
      styles[`${ownerState.variant}Size${capitalize(ownerState.size)}`],
      ownerState.color === "inherit" && styles.colorInherit,
      ownerState.disableElevation && styles.disableElevation,
      ownerState.fullWidth && styles.fullWidth,
    ];
  },
})(
  ({ theme, ownerState }) => ({
    ...theme.typography.button,
    minWidth: 64,
    padding: "6px 16px",
    borderRadius: theme.shape.borderRadius,
    transition: theme.transitions.create(
      ["background-color", "box-shadow", "border-color", "color"],
      {
        duration: theme.transitions.duration.short,
      },
    ),
    "&:hover": {
      textDecoration: "none",
      backgroundColor: alpha(
        theme.palette.text.primary,
        theme.palette.action.hoverOpacity,
      ),
      // Reset on touch devices, it doesn't add specificity
      "@media (hover: none)": {
        backgroundColor: "transparent",
      },
      ...(ownerState.variant === "text" &&
        ownerState.color !== "inherit" && {
          backgroundColor: alpha(
            theme.palette[ownerState.color].main,
            theme.palette.action.hoverOpacity,
          ),
          // Reset on touch devices, it doesn't add specificity
          "@media (hover: none)": {
            backgroundColor: "transparent",
          },
        }),
      ...(ownerState.variant === "outlined" &&
        ownerState.color !== "inherit" && {
          border: `1px solid ${theme.palette[ownerState.color].main}`,
          backgroundColor: alpha(
            theme.palette[ownerState.color].main,
            theme.palette.action.hoverOpacity,
          ),
          // Reset on touch devices, it doesn't add specificity
          "@media (hover: none)": {
            backgroundColor: "transparent",
          },
        }),
      ...(ownerState.variant === "contained" && {
        backgroundColor: theme.palette.grey.A100,
        boxShadow: theme.shadows[4],
        // Reset on touch devices, it doesn't add specificity
        "@media (hover: none)": {
          boxShadow: theme.shadows[2],
          backgroundColor: theme.palette.grey[300],
        },
      }),
      ...(ownerState.variant === "contained" &&
        ownerState.color !== "inherit" && {
          backgroundColor: theme.palette[ownerState.color].dark,
          // Reset on touch devices, it doesn't add specificity
          "@media (hover: none)": {
            backgroundColor: theme.palette[ownerState.color].main,
          },
        }),
    },
    "&:active": {
      ...(ownerState.variant === "contained" && {
        boxShadow: theme.shadows[8],
      }),
    },
    [`&.${buttonClasses.focusVisible}`]: {
      ...(ownerState.variant === "contained" && {
        boxShadow: theme.shadows[6],
      }),
    },
    [`&.${buttonClasses.disabled}`]: {
      color: theme.palette.action.disabled,
      ...(ownerState.variant === "outlined" && {
        border: `1px solid ${theme.palette.action.disabledBackground}`,
      }),
      ...(ownerState.variant === "outlined" &&
        ownerState.color === "secondary" && {
          border: `1px solid ${theme.palette.action.disabled}`,
        }),
      ...(ownerState.variant === "contained" && {
        color: theme.palette.action.disabled,
        boxShadow: theme.shadows[0],
        backgroundColor: theme.palette.action.disabledBackground,
      }),
    },
    ...(ownerState.variant === "text" && {
      padding: "6px 8px",
    }),
    ...(ownerState.variant === "text" &&
      ownerState.color !== "inherit" && {
        color: theme.palette[ownerState.color].main,
      }),
    ...(ownerState.variant === "outlined" && {
      padding: "5px 15px",
      border: `1px solid ${
        theme.palette.mode === "light"
          ? "rgba(0, 0, 0, 0.23)"
          : "rgba(255, 255, 255, 0.23)"
      }`,
    }),
    ...(ownerState.variant === "outlined" &&
      ownerState.color !== "inherit" && {
        color: theme.palette[ownerState.color].main,
        border: `1px solid ${alpha(theme.palette[ownerState.color].main, 0.5)}`,
      }),
    ...(ownerState.variant === "contained" && {
      color: theme.palette.getContrastText(theme.palette.grey[300]),
      backgroundColor: theme.palette.grey[300],
      boxShadow: theme.shadows[2],
    }),
    ...(ownerState.variant === "contained" &&
      ownerState.color !== "inherit" && {
        color: theme.palette[ownerState.color].contrastText,
        backgroundColor: theme.palette[ownerState.color].main,
      }),
    ...(ownerState.color === "inherit" && {
      color: "inherit",
      borderColor: "currentColor",
    }),
    ...(ownerState.size === "small" &&
      ownerState.variant === "text" && {
        padding: "4px 5px",
        fontSize: theme.typography.pxToRem(13),
      }),
    ...(ownerState.size === "large" &&
      ownerState.variant === "text" && {
        padding: "8px 11px",
        fontSize: theme.typography.pxToRem(15),
      }),
    ...(ownerState.size === "small" &&
      ownerState.variant === "outlined" && {
        padding: "3px 9px",
        fontSize: theme.typography.pxToRem(13),
      }),
    ...(ownerState.size === "large" &&
      ownerState.variant === "outlined" && {
        padding: "7px 21px",
        fontSize: theme.typography.pxToRem(15),
      }),
    ...(ownerState.size === "small" &&
      ownerState.variant === "contained" && {
        padding: "4px 10px",
        fontSize: theme.typography.pxToRem(13),
      }),
    ...(ownerState.size === "large" &&
      ownerState.variant === "contained" && {
        padding: "8px 22px",
        fontSize: theme.typography.pxToRem(15),
      }),
    ...(ownerState.fullWidth && {
      width: "100%",
    }),
  }),
  ({ ownerState }) => ({
    ...(ownerState.disableElevation && {
      boxShadow: "none",
      "&:hover": {
        boxShadow: "none",
      },
      [`&.${buttonClasses.focusVisible}`]: {
        boxShadow: "none",
      },
      "&:active": {
        boxShadow: "none",
      },
      [`&.${buttonClasses.disabled}`]: {
        boxShadow: "none",
      },
    }),
  }),
);
const ButtonStartIcon = styled$1("span", {
  name: "MuiButton",
  slot: "StartIcon",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.startIcon, styles[`iconSize${capitalize(ownerState.size)}`]];
  },
})(({ ownerState }) => ({
  display: "inherit",
  marginRight: 8,
  marginLeft: -4,
  ...(ownerState.size === "small" && {
    marginLeft: -2,
  }),
  ...commonIconStyles(ownerState),
}));
const ButtonEndIcon = styled$1("span", {
  name: "MuiButton",
  slot: "EndIcon",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.endIcon, styles[`iconSize${capitalize(ownerState.size)}`]];
  },
})(({ ownerState }) => ({
  display: "inherit",
  marginRight: -4,
  marginLeft: 8,
  ...(ownerState.size === "small" && {
    marginRight: -2,
  }),
  ...commonIconStyles(ownerState),
}));

/**
 *
 * Demos:
 *
 * - [Button Group](https://mui.com/components/button-group/)
 * - [Buttons](https://mui.com/components/buttons/)
 *
 * API:
 *
 * - [Button API](https://mui.com/api/button/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */
const Button = $$1c.component(function Button({
  allProps,
  otherProps,
  props,
  classes,
}) {
  const contextProps = useContext(ButtonGroupContext);
  return createComponent(
    ButtonRoot,
    mergeProps(
      {
        ownerState: allProps,
        get ["class"]() {
          return clsx(classes.root, otherProps.class, contextProps.class);
        },
        get disabled() {
          return props.disabled;
        },
        get focusRipple() {
          return !props.disableFocusRipple;
        },
        get focusVisibleClassName() {
          return clsx(
            props.classes?.focusVisible,
            otherProps.focusVisibleClassName,
          );
        },
        get type() {
          return otherProps.type;
        },
        get href() {
          return props.href;
        },
      },
      otherProps,
      {
        get classes() {
          return props.classes;
        },
        get children() {
          return [
            createComponent(Show, {
              get when() {
                return props.startIcon;
              },
              children: (startIcon) =>
                createComponent(ButtonStartIcon, {
                  get ["class"]() {
                    return classes.startIcon;
                  },
                  ownerState: allProps,
                  get children() {
                    return startIcon();
                  },
                }),
            }),
            createMemo(() => props.children),
            createComponent(Show, {
              get when() {
                return props.endIcon;
              },
              children: (endIcon) =>
                createComponent(ButtonEndIcon, {
                  get ["class"]() {
                    return classes.endIcon;
                  },
                  ownerState: allProps,
                  get children() {
                    return endIcon();
                  },
                }),
            }),
          ];
        },
      },
    ),
  );
});

function getButtonGroupUtilityClass(slot) {
  return generateUtilityClass("MuiButtonGroup", slot);
}
const buttonGroupClasses = generateUtilityClasses("MuiButtonGroup", [
  "root",
  "contained",
  "outlined",
  "text",
  "disableElevation",
  "disabled",
  "fullWidth",
  "vertical",
  "grouped",
  "groupedHorizontal",
  "groupedVertical",
  "groupedText",
  "groupedTextHorizontal",
  "groupedTextVertical",
  "groupedTextPrimary",
  "groupedTextSecondary",
  "groupedOutlined",
  "groupedOutlinedHorizontal",
  "groupedOutlinedVertical",
  "groupedOutlinedPrimary",
  "groupedOutlinedSecondary",
  "groupedContained",
  "groupedContainedHorizontal",
  "groupedContainedVertical",
  "groupedContainedPrimary",
  "groupedContainedSecondary",
]);

const $$1b = createComponentFactory()({
  name: "MuiButtonGroup",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disableElevation",
    "disableFocusRipple",
    "disableRipple",
    "disabled",
    "fullWidth",
    "orientation",
    "size",
    "variant",
  ],
  propDefaults: ({ set }) =>
    set({
      color: "primary",
      component: "div",
      disabled: false,
      disableElevation: false,
      disableFocusRipple: false,
      disableRipple: false,
      fullWidth: false,
      orientation: "horizontal",
      size: "medium",
      variant: "outlined",
    }),
  utilityClass: getButtonGroupUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      ownerState.orientation === "vertical" && "vertical",
      ownerState.fullWidth && "fullWidth",
      ownerState.disableElevation && "disableElevation",
    ],
    grouped: [
      "grouped",
      `grouped${capitalize(ownerState.orientation)}`,
      `grouped${capitalize(ownerState.variant)}`,
      `grouped${capitalize(ownerState.variant)}${capitalize(
        ownerState.orientation,
      )}`,
      `grouped${capitalize(ownerState.variant)}${capitalize(ownerState.color)}`,
      ownerState.disabled && "disabled",
    ],
  }),
});
const ButtonGroupRoot = styled$1("div", {
  name: "MuiButtonGroup",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      {
        [`& .${buttonGroupClasses.grouped}`]: styles.grouped,
      },
      {
        [`& .${buttonGroupClasses.grouped}`]:
          styles[`grouped${capitalize(ownerState.orientation)}`],
      },
      {
        [`& .${buttonGroupClasses.grouped}`]:
          styles[`grouped${capitalize(ownerState.variant)}`],
      },
      {
        [`& .${buttonGroupClasses.grouped}`]:
          styles[
            `grouped${capitalize(ownerState.variant)}${capitalize(
              ownerState.orientation,
            )}`
          ],
      },
      {
        [`& .${buttonGroupClasses.grouped}`]:
          styles[
            `grouped${capitalize(ownerState.variant)}${capitalize(
              ownerState.color,
            )}`
          ],
      },
      styles.root,
      styles[ownerState.variant],
      ownerState.disableElevation === true && styles.disableElevation,
      ownerState.fullWidth && styles.fullWidth,
      ownerState.orientation === "vertical" && styles.vertical,
    ];
  },
})(({ theme, ownerState }) => ({
  display: "inline-flex",
  borderRadius: theme.shape.borderRadius,
  ...(ownerState.variant === "contained" && {
    boxShadow: theme.shadows[2],
  }),
  ...(ownerState.disableElevation && {
    boxShadow: "none",
  }),
  ...(ownerState.fullWidth && {
    width: "100%",
  }),
  ...(ownerState.orientation === "vertical" && {
    flexDirection: "column",
  }),
  [`& .${buttonGroupClasses.grouped}`]: {
    minWidth: 40,
    "&:not(:first-of-type)": {
      ...(ownerState.orientation === "horizontal" && {
        borderTopLeftRadius: 0,
        borderBottomLeftRadius: 0,
      }),
      ...(ownerState.orientation === "vertical" && {
        borderTopRightRadius: 0,
        borderTopLeftRadius: 0,
      }),
      ...(ownerState.variant === "outlined" &&
        ownerState.orientation === "horizontal" && {
          marginLeft: -1,
        }),
      ...(ownerState.variant === "outlined" &&
        ownerState.orientation === "vertical" && {
          marginTop: -1,
        }),
    },
    "&:not(:last-of-type)": {
      ...(ownerState.orientation === "horizontal" && {
        borderTopRightRadius: 0,
        borderBottomRightRadius: 0,
      }),
      ...(ownerState.orientation === "vertical" && {
        borderBottomRightRadius: 0,
        borderBottomLeftRadius: 0,
      }),
      ...(ownerState.variant === "text" &&
        ownerState.orientation === "horizontal" && {
          borderRight: `1px solid ${
            theme.palette.mode === "light"
              ? "rgba(0, 0, 0, 0.23)"
              : "rgba(255, 255, 255, 0.23)"
          }`,
        }),
      ...(ownerState.variant === "text" &&
        ownerState.orientation === "vertical" && {
          borderBottom: `1px solid ${
            theme.palette.mode === "light"
              ? "rgba(0, 0, 0, 0.23)"
              : "rgba(255, 255, 255, 0.23)"
          }`,
        }),
      ...(ownerState.variant === "text" &&
        ownerState.color !== "inherit" && {
          borderColor: alpha(theme.palette[ownerState.color].main, 0.5),
        }),
      ...(ownerState.variant === "outlined" &&
        ownerState.orientation === "horizontal" && {
          borderRightColor: "transparent",
        }),
      ...(ownerState.variant === "outlined" &&
        ownerState.orientation === "vertical" && {
          borderBottomColor: "transparent",
        }),
      ...(ownerState.variant === "contained" &&
        ownerState.orientation === "horizontal" && {
          borderRight: `1px solid ${theme.palette.grey[400]}`,
          [`&.${buttonGroupClasses.disabled}`]: {
            borderRight: `1px solid ${theme.palette.action.disabled}`,
          },
        }),
      ...(ownerState.variant === "contained" &&
        ownerState.orientation === "vertical" && {
          borderBottom: `1px solid ${theme.palette.grey[400]}`,
          [`&.${buttonGroupClasses.disabled}`]: {
            borderBottom: `1px solid ${theme.palette.action.disabled}`,
          },
        }),
      ...(ownerState.variant === "contained" &&
        ownerState.color !== "inherit" && {
          borderColor: theme.palette[ownerState.color].dark,
        }),
      "&:hover": {
        ...(ownerState.variant === "outlined" &&
          ownerState.orientation === "horizontal" && {
            borderRightColor: "currentColor",
          }),
        ...(ownerState.variant === "outlined" &&
          ownerState.orientation === "vertical" && {
            borderBottomColor: "currentColor",
          }),
      },
    },
    "&:hover": {
      ...(ownerState.variant === "contained" && {
        boxShadow: "none",
      }),
    },
    ...(ownerState.variant === "contained" && {
      boxShadow: "none",
    }),
  },
}));

/**
 *
 * Demos:
 *
 * - [Button Group](https://mui.com/components/button-group/)
 *
 * API:
 *
 * - [ButtonGroup API](https://mui.com/api/button-group/)
 */
const ButtonGroup = $$1b.component(function ButtonGroup({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const context = () => ({
    class: classes.grouped,
    color: props.color,
    disabled: props.disabled,
    disableElevation: props.disableElevation,
    disableFocusRipple: props.disableFocusRipple,
    disableRipple: props.disableRipple,
    fullWidth: props.fullWidth,
    size: props.size,
    variant: props.variant,
  });
  return createComponent(
    ButtonGroupRoot,
    mergeProps(
      {
        get as() {
          return otherProps.component;
        },
        role: "group",
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        ownerState: allProps,
      },
      otherProps,
      {
        get children() {
          return createComponent(ButtonGroupContext.Provider, {
            get value() {
              return context();
            },
            get children() {
              return props.children;
            },
          });
        },
      },
    ),
  );
});

function getCardUtilityClass(slot) {
  return generateUtilityClass("MuiCard", slot);
}
generateUtilityClasses("MuiCard", ["root"]);

const $$1a = createComponentFactory()({
  name: "MuiCard",
  selfPropNames: ["classes", "raised"],
  utilityClass: getCardUtilityClass,
  propDefaults: ({ set }) =>
    set({
      component: Paper,
      raised: false,
    }),
  slotClasses: () => ({
    root: ["root"],
  }),
});
const CardRoot = styled$1(Paper, {
  name: "MuiCard",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})(() => {
  return {
    overflow: "hidden",
  };
});

/**
 *
 * Demos:
 *
 * - [Cards](https://mui.com/components/cards/)
 *
 * API:
 *
 * - [Card API](https://mui.com/api/card/)
 * - inherits [Paper API](https://mui.com/api/paper/)
 */

const Card = $$1a.component(function Card({ props, otherProps, classes }) {
  return createComponent(
    CardRoot,
    mergeProps(
      {
        get elevation() {
          return props.raised ? 8 : undefined;
        },
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
      },
      otherProps,
    ),
  );
});

function getCardActionAreaUtilityClass(slot) {
  return generateUtilityClass("MuiCardActionArea", slot);
}
const cardActionAreaClasses = generateUtilityClasses("MuiCardActionArea", [
  "root",
  "focusVisible",
  "focusHighlight",
]);

const $$19 = createComponentFactory()({
  name: "MuiCardActionArea",
  selfPropNames: ["classes", "focusVisibleClassName"],
  utilityClass: getCardActionAreaUtilityClass,
  slotClasses: () => ({
    root: ["root"],
    focusHighlight: ["focusHighlight"],
    focusVisible: ["focusVisible"],
  }),
});
const CardActionAreaRoot = styled$1(ButtonBase, {
  name: "MuiCardActionArea",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})(({ theme }) => ({
  display: "block",
  textAlign: "inherit",
  width: "100%",
  [`&:hover .${cardActionAreaClasses.focusHighlight}`]: {
    opacity: theme.palette.action.hoverOpacity,
    "@media (hover: none)": {
      opacity: 0,
    },
  },
  [`&.${cardActionAreaClasses.focusVisible} .${cardActionAreaClasses.focusHighlight}`]:
    {
      opacity: theme.palette.action.focusOpacity,
    },
}));
const CardActionAreaFocusHighlight = styled$1("span", {
  name: "MuiCardActionArea",
  slot: "FocusHighlight",
  overridesResolver: (props, styles) => styles.focusHighlight,
})(({ theme }) => ({
  overflow: "hidden",
  pointerEvents: "none",
  position: "absolute",
  top: 0,
  right: 0,
  bottom: 0,
  left: 0,
  borderRadius: "inherit",
  opacity: 0,
  backgroundColor: "currentcolor",
  transition: theme.transitions.create("opacity", {
    duration: theme.transitions.duration.short,
  }),
}));

/**
 *
 * Demos:
 *
 * - [Cards](https://mui.com/components/cards/)
 *
 * API:
 *
 * - [CardActionArea API](https://mui.com/api/card-action-area/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */

const CardActionArea = $$19.component(function CardActionArea({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    CardActionAreaRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      get focusVisibleClassName() {
        return clsx(props.focusVisibleClassName, classes.focusVisible);
      },
      ownerState: allProps,
      get children() {
        return [
          createMemo(() => otherProps.children),
          createComponent(CardActionAreaFocusHighlight, {
            get ["class"]() {
              return classes.focusHighlight;
            },
            ownerState: allProps,
          }),
        ];
      },
    }),
  );
});

function getCardActionsUtilityClass(slot) {
  return generateUtilityClass("MuiCardActions", slot);
}
generateUtilityClasses("MuiCardActions", ["root", "spacing"]);

const $$18 = createComponentFactory()({
  name: "MuiActions",
  selfPropNames: ["children", "classes", "disableSpacing"],
  propDefaults: ({ set }) =>
    set({
      disableSpacing: false,
    }),
  utilityClass: getCardActionsUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", !ownerState.disableSpacing && "spacing"],
  }),
});
const CardActionsRoot = styled$1("div", {
  name: "MuiCardActions",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.root, !ownerState.disableSpacing && styles.spacing];
  },
})(({ ownerState }) => ({
  display: "flex",
  alignItems: "center",
  padding: 8,
  ...(!ownerState.disableSpacing && {
    "& > :not(:first-of-type)": {
      marginLeft: 8,
    },
  }),
}));

/**
 *
 * Demos:
 *
 * - [Cards](https://mui.com/components/cards/)
 *
 * API:
 *
 * - [CardActions API](https://mui.com/api/card-actions/)
 */
const CardActions = $$18.component(function CardActions({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    CardActionsRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: allProps,
      get children() {
        return props.children;
      },
    }),
  );
});

function getCardContentUtilityClass(slot) {
  return generateUtilityClass("MuiCardContent", slot);
}
generateUtilityClasses("MuiCardContent", ["root"]);

const $$17 = createComponentFactory()({
  name: "MuiContent",
  selfPropNames: ["children", "classes"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
    }),
  utilityClass: getCardContentUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const CardContentRoot = styled$1("div", {
  name: "MuiCardContent",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})(() => {
  return {
    padding: 16,
    "&:last-child": {
      paddingBottom: 24,
    },
  };
});

/**
 *
 * Demos:
 *
 * - [Cards](https://mui.com/components/cards/)
 *
 * API:
 *
 * - [CardContent API](https://mui.com/api/card-content/)
 */
const CardContent = $$17.component(function CardContent({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    CardContentRoot,
    mergeProps(otherProps, {
      ownerState: allProps,
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      get children() {
        return props.children;
      },
    }),
  );
});

function getCardHeaderUtilityClass(slot) {
  return generateUtilityClass("MuiCardHeader", slot);
}
const cardHeaderClasses = generateUtilityClasses("MuiCardHeader", [
  "root",
  "avatar",
  "action",
  "content",
  "title",
  "subheader",
]);

const $$16 = createComponentFactory()({
  name: "MuiCardHeader",
  selfPropNames: [
    "action",
    "avatar",
    "classes",
    "disableTypography",
    "subheader",
    "subheaderTypographyProps",
    "title",
    "titleTypographyProps",
  ],
  propDefaults: ({ set }) =>
    set({
      component: "div",
      disableTypography: false,
    }),
  utilityClass: getCardHeaderUtilityClass,
  slotClasses: () => ({
    root: ["root"],
    avatar: ["avatar"],
    action: ["action"],
    content: ["content"],
    title: ["title"],
    subheader: ["subheader"],
  }),
});
const CardHeaderRoot = styled$1("div", {
  name: "MuiCardHeader",
  slot: "Root",
  overridesResolver: (props, styles) => ({
    [`& .${cardHeaderClasses.title}`]: styles.title,
    [`& .${cardHeaderClasses.subheader}`]: styles.subheader,
    ...styles.root,
  }),
})({
  display: "flex",
  alignItems: "center",
  padding: 16,
});
const CardHeaderAvatar = styled$1("div", {
  name: "MuiCardHeader",
  slot: "Avatar",
  overridesResolver: (props, styles) => styles.avatar,
})({
  display: "flex",
  flex: "0 0 auto",
  marginRight: 16,
});
const CardHeaderAction = styled$1("div", {
  name: "MuiCardHeader",
  slot: "Action",
  overridesResolver: (props, styles) => styles.action,
})({
  flex: "0 0 auto",
  alignSelf: "flex-start",
  marginTop: -4,
  marginRight: -8,
  marginBottom: -4,
});
const CardHeaderContent = styled$1("div", {
  name: "MuiCardHeader",
  slot: "Content",
  overridesResolver: (props, styles) => styles.content,
})({
  flex: "1 1 auto",
});

/**
 *
 * Demos:
 *
 * - [Cards](https://mui.com/components/cards/)
 *
 * API:
 *
 * - [CardHeader API](https://mui.com/api/card-header/)
 */
const CardHeader = $$16.component(function CardHeader({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const avatar = children(() => props.avatar);
  const Avatar = () => {
    return createComponent(Show, {
      get when() {
        return !!avatar();
      },
      get children() {
        return createComponent(CardHeaderAvatar, {
          get ["class"]() {
            return classes.avatar;
          },
          ownerState: allProps,
          get children() {
            return avatar();
          },
        });
      },
    });
  };
  const Title = () => {
    const title = children(() => props.title);
    return createComponent(Show, {
      get when() {
        return (
          title() !== undefined &&
          !isSuidElement(title(), Typography) &&
          !props.disableTypography
        );
      },
      get fallback() {
        return title();
      },
      get children() {
        return createComponent(
          Typography,
          mergeProps(
            {
              get variant() {
                return avatar() ? "body2" : "h5";
              },
              get ["class"]() {
                return classes.title;
              },
              component: "span",
              sx: {
                display: "block",
              },
            },
            () => props.titleTypographyProps,
            {
              get children() {
                return title();
              },
            },
          ),
        );
      },
    });
  };
  const Subheader = () => {
    const subheader = children(() => props.subheader);
    return createComponent(Show, {
      get when() {
        return (
          subheader() !== undefined &&
          !isSuidElement(subheader(), Typography) &&
          !props.disableTypography
        );
      },
      get fallback() {
        return subheader();
      },
      get children() {
        return createComponent(
          Typography,
          mergeProps(
            {
              get variant() {
                return avatar() ? "body2" : "body1";
              },
              get ["class"]() {
                return classes.subheader;
              },
              component: "span",
              sx: {
                display: "block",
                color: "text.secondary",
              },
            },
            () => props.subheaderTypographyProps,
            {
              get children() {
                return subheader();
              },
            },
          ),
        );
      },
    });
  };
  const Action = () => {
    const action = children(() => props.action);
    return createComponent(Show, {
      get when() {
        return !!action();
      },
      get children() {
        return createComponent(CardHeaderAction, {
          get ["class"]() {
            return classes.action;
          },
          ownerState: allProps,
          get children() {
            return action();
          },
        });
      },
    });
  };
  return createComponent(
    CardHeaderRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: allProps,
      get children() {
        return [
          createComponent(Avatar, {}),
          createComponent(CardHeaderContent, {
            get ["class"]() {
              return classes.content;
            },
            ownerState: allProps,
            get children() {
              return [
                createComponent(Title, {}),
                createComponent(Subheader, {}),
              ];
            },
          }),
          createComponent(Action, {}),
        ];
      },
    }),
  );
});

function getCardMediaUtilityClass(slot) {
  return generateUtilityClass("MuiCardMedia", slot);
}
generateUtilityClasses("MuiCardMedia", ["root", "media", "img"]);

const $$15 = createComponentFactory()({
  name: "MuiCardMedia",
  selfPropNames: ["children", "classes", "image", "src"],
  utilityClass: getCardMediaUtilityClass,
  propDefaults: ({ set }) =>
    set({
      component: "div",
    }),
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.isMediaComponent && "media",
      ownerState.isImageComponent && "img",
    ],
  }),
});
const CardMediaRoot = styled$1("div", {
  name: "MuiCardMedia",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    const { isMediaComponent, isImageComponent } = ownerState;
    return [
      styles.root,
      isMediaComponent && styles.media,
      isImageComponent && styles.img,
    ];
  },
})(({ ownerState }) => ({
  display: "block",
  backgroundSize: "cover",
  backgroundRepeat: "no-repeat",
  backgroundPosition: "center",
  ...(ownerState.isMediaComponent && {
    width: "100%",
  }),
  ...(ownerState.isImageComponent && {
    // ⚠️ object-fit is not supported by IE11.
    objectFit: "cover",
  }),
}));
const MEDIA_COMPONENTS = ["video", "audio", "picture", "iframe", "img"];
const IMAGE_COMPONENTS = ["picture", "img"];

/**
 *
 * Demos:
 *
 * - [Cards](https://mui.com/components/cards/)
 *
 * API:
 *
 * - [CardMedia API](https://mui.com/api/card-media/)
 */
const CardMedia = $$15.component(function CardMedia({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const isMediaComponent = createMemo(
    () => MEDIA_COMPONENTS.indexOf(otherProps.component) !== -1,
  );
  const style = () =>
    !isMediaComponent() && props.image
      ? mergeProps(
          {
            get "background-image"() {
              return `url("${props.image}")`;
            },
          },
          typeof otherProps.style === "object" && otherProps.style
            ? otherProps.style
            : {},
        )
      : otherProps.style;
  const ownerState = mergeProps(allProps, {
    get isMediaComponent() {
      return isMediaComponent();
    },
    get isImageComponent() {
      return IMAGE_COMPONENTS.indexOf(otherProps.component) !== -1;
    },
  });
  const $CardMediaRoot = redefine(CardMediaRoot, "div", "img");
  return createComponent(
    $CardMediaRoot,
    mergeProps(
      {
        get role() {
          return !isMediaComponent() && props.image ? "img" : undefined;
        },
      },
      otherProps,
      {
        get as() {
          return otherProps.component || otherProps.as;
        },
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        get style() {
          return style();
        },
        ownerState: ownerState,
        get src() {
          return isMediaComponent() ? props.image || props.src : undefined;
        },
        get children() {
          return props.children;
        },
      },
    ),
  );
});

const FormControlContext = createContext();

function useFormControl() {
  return useContext(FormControlContext);
}

const FormControlLabelContext = createContext();

function useFormControlLabel() {
  return useContext(FormControlLabelContext);
}

function useControlled(props) {
  const isControlled = props.controlled() !== void 0;
  const [valueState, setValue] = createSignal(props.default());
  const value = createMemo(() =>
    isControlled ? props.controlled() : valueState(),
  );
  if (isControlled)
    createEffect(() => {
      setValue(() => value());
    });
  const setValueIfUncontrolled = (newValue) => {
    if (!isControlled) {
      setValue(newValue);
    }
  };
  return [value, setValueIfUncontrolled];
}

function getSwitchBaseUtilityClass(slot) {
  return generateUtilityClass("PrivateSwitchBase", slot);
}
generateUtilityClasses("PrivateSwitchBase", [
  "root",
  "checked",
  "disabled",
  "input",
  "edgeStart",
  "edgeEnd",
]);

const $$14 = createComponentFactory()({
  name: "MuiSwitchBase",
  selfPropNames: [
    "autoFocus",
    "checked",
    "checkedIcon",
    "classes",
    "defaultChecked",
    "disableFocusRipple",
    "disableRipple",
    "disabled",
    "edge",
    "icon",
    "id",
    "inputProps",
    "inputRef",
    "name",
    "onChange",
    "readOnly",
    "required",
    "tabIndex",
    "type",
    "value",
  ],
  autoCallUseClasses: false,
  propDefaults: ({ set }) =>
    set({
      disableFocusRipple: false,
      edge: false,
    }),
  utilityClass: getSwitchBaseUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !!ownerState.checked && "checked",
      !!ownerState.disabled && "disabled",
      ownerState.edge && `edge${capitalize(ownerState.edge)}`,
    ],
    input: ["input"],
  }),
});
const SwitchBaseRoot = styled$1(ButtonBase)(({ ownerState }) => ({
  padding: 9,
  borderRadius: "50%",
  ...(ownerState.edge === "start" && {
    marginLeft: ownerState.size === "small" ? -3 : -12,
  }),
  ...(ownerState.edge === "end" && {
    marginRight: ownerState.size === "small" ? -3 : -12,
  }),
}));
const SwitchBaseInput = styled$1("input")({
  cursor: "inherit",
  position: "absolute",
  opacity: 0,
  width: "100%",
  height: "100%",
  top: 0,
  left: 0,
  margin: 0,
  padding: 0,
  zIndex: 1,
});

/**
 * @ignore - internal component.
 */
const SwitchBase = $$14.component(function SwitchBase({
  allProps,
  otherProps,
  props,
}) {
  const formControlLabel = useFormControlLabel();
  const [checked, setCheckedState] = useControlled({
    controlled: () => props.checked ?? formControlLabel?.checked,
    default: () => Boolean(props.defaultChecked),
    name: "SwitchBase",
    state: "checked",
  });
  const muiFormControl = useFormControl();
  const disabled = () => {
    if (typeof formControlLabel?.disabled !== "undefined") {
      return formControlLabel.disabled;
    } else if (muiFormControl && typeof props.disabled === "undefined") {
      return muiFormControl.disabled;
    } else {
      return props.disabled;
    }
  };
  createEffect(() => {
    if (formControlLabel) {
      formControlLabel.setDisabled(!!disabled());
    }
  });
  const hasLabelFor = () => props.type === "checkbox" || props.type === "radio";
  const ownerState = mergeProps(allProps, {
    get checked() {
      return checked();
    },
    get disabled() {
      return disabled();
    },
  });
  const classes = $$14.useClasses(ownerState);
  const element = createRef(() => props.inputRef);
  const inputValue = () => props.value ?? formControlLabel?.value;
  createEffect(() => {
    if (typeof props.defaultChecked === "boolean")
      element.ref.defaultChecked = props.defaultChecked;
  });
  createEffect(() => {
    const value = checked();
    if (typeof value === "boolean") element.ref.checked = value;
  });
  return createComponent(
    SwitchBaseRoot,
    mergeProps(otherProps, {
      component: "span",
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      centerRipple: true,
      get focusRipple() {
        return !props.disableFocusRipple;
      },
      get disabled() {
        return disabled();
      },
      tabIndex: null,
      role: undefined,
      onFocus: (event) => {
        if (typeof otherProps.onFocus === "function") {
          otherProps.onFocus(event);
        }
        muiFormControl?.onFocus?.();
      },
      onBlur: (event) => {
        if (typeof otherProps.onBlur === "function") {
          otherProps.onBlur(event);
        }
        muiFormControl?.onBlur?.();
      },
      ownerState: ownerState,
      get children() {
        return [
          createComponent(
            SwitchBaseInput,
            mergeProps(
              {
                as: "input",
                get autofocus() {
                  return props.autoFocus;
                },
                get ["class"]() {
                  return classes.input;
                },
                get disabled() {
                  return disabled();
                },
                get id() {
                  return hasLabelFor() ? props.id : undefined;
                },
                get name() {
                  return props.name ?? formControlLabel?.name;
                },
                onClick: (event) => {
                  // Workaround for https://github.com/facebook/react/issues/9023
                  if (event.defaultPrevented) {
                    return;
                  }
                  const newChecked = event.currentTarget.checked;
                  // Revert state immediately
                  event.currentTarget.checked = !newChecked;
                  setCheckedState(newChecked);
                  if (typeof props.onChange === "function") {
                    // TODO v6: remove the second argument.
                    props.onChange(event, newChecked);
                  } else {
                    formControlLabel?.onChange?.(event, newChecked);
                  }
                  if (typeof otherProps.onClick === "function")
                    otherProps.onClick(event);
                },
                get readOnly() {
                  return props.readOnly;
                },
                ref: (e) => {
                  element(e);
                  if (!props.inputRef) formControlLabel?.inputRef?.(e);
                },
                get required() {
                  return props.required;
                },
                ownerState: ownerState,
                get tabIndex() {
                  return props.tabIndex;
                },
                get type() {
                  return props.type;
                },
                get value() {
                  return inputValue();
                },
              },
              () => props.inputProps || {},
            ),
          ),
          createMemo(() => (checked() ? props.checkedIcon : props.icon)),
          createMemo(() => otherProps.children),
        ];
      },
    }),
  );
});

const _tmpl$$c = /*#__PURE__*/ template(
  `<svg><path d="M19 3H5c-1.11 0-2 .9-2 2v14c0 1.1.89 2 2 2h14c1.11 0 2-.9 2-2V5c0-1.1-.89-2-2-2zm-9 14l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const CheckBoxIcon = createSvgIcon(() => _tmpl$$c(), "CheckBox");

const _tmpl$$b = /*#__PURE__*/ template(
  `<svg><path d="M19 5v14H5V5h14m0-2H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const CheckBoxOutlineBlankIcon = createSvgIcon(
  () => _tmpl$$b(),
  "CheckBoxOutlineBlank",
);

const _tmpl$$a = /*#__PURE__*/ template(
  `<svg><path d="M19 3H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm-2 10H7v-2h10v2z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const IndeterminateCheckBoxIcon = createSvgIcon(
  () => _tmpl$$a(),
  "IndeterminateCheckBox",
);

function getCheckboxUtilityClass(slot) {
  return generateUtilityClass("MuiCheckbox", slot);
}
const checkboxClasses = generateUtilityClasses("MuiCheckbox", [
  "root",
  "checked",
  "disabled",
  "indeterminate",
  "colorPrimary",
  "colorSecondary",
]);

const $$13 = createComponentFactory()({
  name: "MuiCheckbox",
  propDefaults: ({ set }) =>
    set({
      color: "primary",
      indeterminate: false,
      size: "medium",
      checkedIcon: () => createComponent(CheckBoxIcon, {}),
      icon: () => createComponent(CheckBoxOutlineBlankIcon, {}),
      indeterminateIcon: () => createComponent(IndeterminateCheckBoxIcon, {}),
    }),
  selfPropNames: [
    "checked",
    "checkedIcon",
    "classes",
    "color",
    "disableRipple",
    "disabled",
    "icon",
    "id",
    "indeterminate",
    "indeterminateIcon",
    "inputProps",
    "inputRef",
    "onChange",
    "required",
    "size",
    "value",
  ],
  utilityClass: getCheckboxUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.indeterminate && "indeterminate",
      `color${capitalize(ownerState.color)}`,
    ],
  }),
});
const CheckboxRoot = styled$1(SwitchBase, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiCheckbox",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.indeterminate && styles.indeterminate,
      ownerState.color !== "default" &&
        styles[`color${capitalize(ownerState.color)}`],
    ];
  },
})(({ theme, ownerState }) => ({
  color: theme.palette.text.secondary,
  ...(!ownerState.disableRipple && {
    "&:hover": {
      backgroundColor: alpha(
        ownerState.color === "default"
          ? theme.palette.action.active
          : theme.palette[ownerState.color].main,
        theme.palette.action.hoverOpacity,
      ),
      // Reset on touch devices, it doesn't add specificity
      "@media (hover: none)": {
        backgroundColor: "transparent",
      },
    },
  }),
  ...(ownerState.color !== "default" && {
    [`&.${checkboxClasses.checked}, &.${checkboxClasses.indeterminate}`]: {
      color: theme.palette[ownerState.color].main,
    },
    [`&.${checkboxClasses.disabled}`]: {
      color: theme.palette.action.disabled,
    },
  }),
}));

/**
 *
 * Demos:
 *
 * - [Checkboxes](https://mui.com/components/checkboxes/)
 * - [Transfer List](https://mui.com/components/transfer-list/)
 *
 * API:
 *
 * - [Checkbox API](https://mui.com/api/checkbox/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */
const Checkbox = $$13.component(function Checkbox({
  allProps,
  classes,
  props,
}) {
  const icon = () =>
    props.indeterminate ? props.indeterminateIcon : props.icon;
  const indeterminateIcon = () =>
    props.indeterminate ? props.indeterminateIcon : props.checkedIcon;
  const [, baseProps] = splitProps(allProps, [
    "checkedIcon",
    "color",
    "icon",
    "indeterminate",
    "indeterminateIcon",
    "inputProps",
    "size",
  ]);
  const allClasses = mergeProps(() => props.classes || {}, classes);
  return createComponent(SvgIconContext.Provider, {
    value: {
      get fontSize() {
        return props.size;
      },
    },
    get children() {
      return createComponent(
        CheckboxRoot,
        mergeProps(
          {
            type: "checkbox",
          },
          baseProps,
          {
            classes: allClasses,
            get inputProps() {
              return {
                ["data-indeterminate"]: props.indeterminate,
                ...(props.inputProps || {}),
              };
            },
            get icon() {
              return icon();
            },
            get checkedIcon() {
              return indeterminateIcon();
            },
            ownerState: allProps,
          },
        ),
      );
    },
  });
});

const _tmpl$$9 = /*#__PURE__*/ template(
  `<svg><path d="M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const CancelIcon = createSvgIcon(() => _tmpl$$9(), "Cancel");

function getChipUtilityClass(slot) {
  return generateUtilityClass("MuiChip", slot);
}
const chipClasses = generateUtilityClasses("MuiChip", [
  "root",
  "sizeSmall",
  "sizeMedium",
  "colorPrimary",
  "colorSecondary",
  "disabled",
  "clickable",
  "clickableColorPrimary",
  "clickableColorSecondary",
  "deletable",
  "deletableColorPrimary",
  "deletableColorSecondary",
  "outlined",
  "filled",
  "outlinedPrimary",
  "outlinedSecondary",
  "avatar",
  "avatarSmall",
  "avatarMedium",
  "avatarColorPrimary",
  "avatarColorSecondary",
  "icon",
  "iconSmall",
  "iconMedium",
  "iconColorPrimary",
  "iconColorSecondary",
  "label",
  "labelSmall",
  "labelMedium",
  "deleteIcon",
  "deleteIconSmall",
  "deleteIconMedium",
  "deleteIconColorPrimary",
  "deleteIconColorSecondary",
  "deleteIconOutlinedColorPrimary",
  "deleteIconOutlinedColorSecondary",
  "focusVisible",
]);

const $$12 = createComponentFactory()({
  name: "MuiChip",
  propDefaults: ({ set }) =>
    set({
      color: "default",
      disabled: false,
      size: "medium",
      variant: "filled",
    }),
  selfPropNames: [
    "avatar",
    "children",
    "classes",
    "clickable",
    "color",
    "deleteIcon",
    "disabled",
    "icon",
    "label",
    "onDelete",
    "size",
    "variant",
  ],
  utilityClass: getChipUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      ownerState.disabled && "disabled",
      `size${capitalize(ownerState.size)}`,
      `color${capitalize(ownerState.color)}`,
      !!ownerState.clickable && "clickable",
      !!ownerState.clickable && `clickableColor${capitalize(ownerState.color)}`,
      !!ownerState.onDelete && "deletable",
      !!ownerState.onDelete && `deletableColor${capitalize(ownerState.color)}`,
      `${ownerState.variant}${capitalize(ownerState.color)}`,
    ],
    label: ["label", `label${capitalize(ownerState.size)}`],
    avatar: [
      "avatar",
      `avatar${capitalize(ownerState.size)}`,
      `avatarColor${capitalize(ownerState.color)}`,
    ],
    icon: [
      "icon",
      `icon${capitalize(ownerState.size)}`,
      `iconColor${capitalize(ownerState.color)}`,
    ],
    deleteIcon: [
      "deleteIcon",
      `deleteIcon${capitalize(ownerState.size)}`,
      `deleteIconColor${capitalize(ownerState.color)}`,
      `deleteIconOutlinedColor${capitalize(ownerState.color)}`,
    ],
  }),
});
const ChipRoot = styled$1("div", {
  name: "MuiChip",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    const { color, clickable, onDelete, size, variant } = ownerState;
    return [
      {
        [`& .${chipClasses.avatar}`]: styles.avatar,
      },
      {
        [`& .${chipClasses.avatar}`]: styles[`avatar${capitalize(size)}`],
      },
      {
        [`& .${chipClasses.avatar}`]: styles[`avatarColor${capitalize(color)}`],
      },
      {
        [`& .${chipClasses.icon}`]: styles.icon,
      },
      {
        [`& .${chipClasses.icon}`]: styles[`icon${capitalize(size)}`],
      },
      {
        [`& .${chipClasses.icon}`]: styles[`iconColor${capitalize(color)}`],
      },
      {
        [`& .${chipClasses.deleteIcon}`]: styles.deleteIcon,
      },
      {
        [`& .${chipClasses.deleteIcon}`]:
          styles[`deleteIcon${capitalize(size)}`],
      },
      {
        [`& .${chipClasses.deleteIcon}`]:
          styles[`deleteIconColor${capitalize(color)}`],
      },
      {
        [`& .${chipClasses.deleteIcon}`]:
          styles[`deleteIconOutlinedColor${capitalize(color)}`],
      },
      styles.root,
      styles[`size${capitalize(size)}`],
      styles[`color${capitalize(color)}`],
      clickable && styles.clickable,
      clickable &&
        color !== "default" &&
        styles[`clickableColor${capitalize(color)})`],
      onDelete && styles.deletable,
      onDelete &&
        color !== "default" &&
        styles[`deletableColor${capitalize(color)}`],
      styles[variant],
      variant === "outlined" && styles[`outlined${capitalize(color)}`],
    ];
  },
})(
  ({ theme, ownerState }) => {
    const deleteIconColor = alpha(theme.palette.text.primary, 0.26);
    return {
      maxWidth: "100%",
      fontFamily: theme.typography.fontFamily,
      fontSize: theme.typography.pxToRem(13),
      display: "inline-flex",
      alignItems: "center",
      justifyContent: "center",
      height: 32,
      color: theme.palette.text.primary,
      backgroundColor: theme.palette.action.selected,
      borderRadius: 32 / 2,
      whiteSpace: "nowrap",
      transition: theme.transitions.create(["background-color", "box-shadow"]),
      // label will inherit this from root, then `clickable` class overrides this for both
      cursor: "default",
      // We disable the focus ring for mouse, touch and keyboard users.
      outline: 0,
      textDecoration: "none",
      border: 0,
      // Remove `button` border
      padding: 0,
      // Remove `button` padding
      verticalAlign: "middle",
      boxSizing: "border-box",
      [`&.${chipClasses.disabled}`]: {
        opacity: theme.palette.action.disabledOpacity,
        pointerEvents: "none",
      },
      [`& .${chipClasses.avatar}`]: {
        marginLeft: 5,
        marginRight: -6,
        width: 24,
        height: 24,
        color:
          theme.palette.mode === "light"
            ? theme.palette.grey[700]
            : theme.palette.grey[300],
        fontSize: theme.typography.pxToRem(12),
      },
      [`& .${chipClasses.avatarColorPrimary}`]: {
        color: theme.palette.primary.contrastText,
        backgroundColor: theme.palette.primary.dark,
      },
      [`& .${chipClasses.avatarColorSecondary}`]: {
        color: theme.palette.secondary.contrastText,
        backgroundColor: theme.palette.secondary.dark,
      },
      [`& .${chipClasses.avatarSmall}`]: {
        marginLeft: 4,
        marginRight: -4,
        width: 18,
        height: 18,
        fontSize: theme.typography.pxToRem(10),
      },
      [`& .${chipClasses.icon}`]: {
        color:
          theme.palette.mode === "light"
            ? theme.palette.grey[700]
            : theme.palette.grey[300],
        marginLeft: 5,
        marginRight: -6,
        ...(ownerState.size === "small" && {
          fontSize: 18,
          marginLeft: 4,
          marginRight: -4,
        }),
        ...(ownerState.color !== "default" && {
          color: "inherit",
        }),
      },
      "--xxxxx": `& .${chipClasses.deleteIcon}`,
      [`& .${chipClasses.deleteIcon}`]: {
        WebkitTapHighlightColor: "transparent",
        color: deleteIconColor,
        fontSize: 22,
        cursor: "pointer",
        margin: "0 5px 0 -6px",
        "&:hover": {
          color: alpha(deleteIconColor, 0.4),
        },
        ...(ownerState.size === "small" && {
          fontSize: 16,
          marginRight: 4,
          marginLeft: -4,
        }),
        ...(ownerState.color !== "default" && {
          color: alpha(theme.palette[ownerState.color].contrastText, 0.7),
          "&:hover, &:active": {
            color: theme.palette[ownerState.color].contrastText,
          },
        }),
      },
      ...(ownerState.size === "small" && {
        height: 24,
      }),
      ...(ownerState.color !== "default" && {
        backgroundColor: theme.palette[ownerState.color].main,
        color: theme.palette[ownerState.color].contrastText,
      }),
      ...(ownerState.onDelete && {
        [`&.${chipClasses.focusVisible}`]: {
          backgroundColor: alpha(
            theme.palette.action.selected,
            theme.palette.action.selectedOpacity +
              theme.palette.action.focusOpacity,
          ),
        },
      }),
      ...(ownerState.onDelete &&
        ownerState.color !== "default" && {
          [`&.${chipClasses.focusVisible}`]: {
            backgroundColor: theme.palette[ownerState.color].dark,
          },
        }),
    };
  },
  ({ theme, ownerState }) => ({
    ...(ownerState.clickable && {
      userSelect: "none",
      WebkitTapHighlightColor: "transparent",
      cursor: "pointer",
      "&:hover": {
        backgroundColor: alpha(
          theme.palette.action.selected,
          theme.palette.action.selectedOpacity +
            theme.palette.action.hoverOpacity,
        ),
      },
      [`&.${chipClasses.focusVisible}`]: {
        backgroundColor: alpha(
          theme.palette.action.selected,
          theme.palette.action.selectedOpacity +
            theme.palette.action.focusOpacity,
        ),
      },
      "&:active": {
        boxShadow: theme.shadows[1],
      },
    }),
    ...(ownerState.clickable &&
      ownerState.color !== "default" && {
        [`&:hover, &.${chipClasses.focusVisible}`]: {
          backgroundColor: theme.palette[ownerState.color].dark,
        },
      }),
  }),
  ({ theme, ownerState }) => ({
    ...(ownerState.variant === "outlined" && {
      backgroundColor: "transparent",
      border: `1px solid ${
        theme.palette.mode === "light"
          ? theme.palette.grey[400]
          : theme.palette.grey[700]
      }`,
      [`&.${chipClasses.clickable}:hover`]: {
        backgroundColor: theme.palette.action.hover,
      },
      [`&.${chipClasses.focusVisible}`]: {
        backgroundColor: theme.palette.action.focus,
      },
      [`& .${chipClasses.avatar}`]: {
        marginLeft: 4,
      },
      [`& .${chipClasses.avatarSmall}`]: {
        marginLeft: 2,
      },
      [`& .${chipClasses.icon}`]: {
        marginLeft: 4,
      },
      [`& .${chipClasses.iconSmall}`]: {
        marginLeft: 2,
      },
      [`& .${chipClasses.deleteIcon}`]: {
        marginRight: 5,
      },
      [`& .${chipClasses.deleteIconSmall}`]: {
        marginRight: 3,
      },
    }),
    ...(ownerState.variant === "outlined" &&
      ownerState.color !== "default" && {
        color: theme.palette[ownerState.color].main,
        border: `1px solid ${alpha(theme.palette[ownerState.color].main, 0.7)}`,
        [`&.${chipClasses.clickable}:hover`]: {
          backgroundColor: alpha(
            theme.palette[ownerState.color].main,
            theme.palette.action.hoverOpacity,
          ),
        },
        [`&.${chipClasses.focusVisible}`]: {
          backgroundColor: alpha(
            theme.palette[ownerState.color].main,
            theme.palette.action.focusOpacity,
          ),
        },
        [`& .${chipClasses.deleteIcon}`]: {
          color: alpha(theme.palette[ownerState.color].main, 0.7),
          "&:hover, &:active": {
            color: theme.palette[ownerState.color].main,
          },
        },
      }),
  }),
);
const ChipLabel = styled$1("span", {
  name: "MuiChip",
  slot: "Label",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    const { size } = ownerState;
    return [styles.label, styles[`label${capitalize(size)}`]];
  },
})(({ ownerState }) => ({
  overflow: "hidden",
  textOverflow: "ellipsis",
  paddingLeft: 12,
  paddingRight: 12,
  whiteSpace: "nowrap",
  ...(ownerState.size === "small" && {
    paddingLeft: 8,
    paddingRight: 8,
  }),
}));
function isDeleteKeyboardEvent(keyboardEvent) {
  return keyboardEvent.key === "Backspace" || keyboardEvent.key === "Delete";
}

/**
 * Chips represent complex entities in small blocks, such as a contact.
 *
 * Demos:
 *
 * - [Chips](https://mui.com/components/chips/)
 *
 * API:
 *
 * - [Chip API](https://mui.com/api/chip/)
 */
const Chip = $$12.component(function Chip({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const element = createElementRef(otherProps);
  const handleDeleteIconClick = (event) => {
    // Stop the event from bubbling up to the `Chip`
    event.stopPropagation();
    props.onDelete?.();
  };
  const handleKeyDown = (event) => {
    // Ignore events from children of `Chip`.
    if (event.currentTarget === event.target && isDeleteKeyboardEvent(event)) {
      // Will be handled in keyUp, otherwise some browsers
      // might init navigation
      event.preventDefault();
    }
    if (typeof otherProps.onKeyDown === "function") otherProps.onKeyDown(event);
  };
  const handleKeyUp = (event) => {
    // Ignore events from children of `Chip`.
    if (event.currentTarget === event.target) {
      if (props.onDelete && isDeleteKeyboardEvent(event)) {
        props.onDelete();
      } else if (event.key === "Escape" && element.ref) {
        element.ref.blur();
      }
    }
    if (typeof otherProps.onKeyUp === "function") otherProps.onKeyUp(event);
  };
  const clickable = () =>
    props.clickable !== false && otherProps.onClick ? true : !!props.clickable;
  const component = () =>
    clickable() || props.onDelete ? ButtonBase : otherProps.component || "div";
  const moreProps = createMemo(() =>
    component() === ButtonBase
      ? {
          component: otherProps.component || "div",
          //focusVisibleClassName: classes.focusVisible,
          ...(props.onDelete && {
            disableRipple: true,
          }),
        }
      : {},
  );
  const deleteIcon = children(() => props.deleteIcon);
  const avatar = children(() => props.avatar);
  const icon = children(() => props.icon);
  const DeleteIcon = () => {
    createRenderEffect(() => {
      const element = deleteIcon();
      if (isElement$1(element)) {
        addElementClass(element, classes.deleteIcon);
      }
    });
    onMount(() => {
      const element = deleteIcon();
      if (element instanceof SVGElement || element instanceof HTMLElement)
        element.onclick = handleDeleteIconClick;
    });
    return createComponent(Show, {
      get when() {
        return props.onDelete;
      },
      get children() {
        return createComponent(Show, {
          get when() {
            return isElement$1(deleteIcon());
          },
          get fallback() {
            return createComponent(CancelIcon, {
              get ["class"]() {
                return classes.deleteIcon;
              },
              onClick: handleDeleteIconClick,
            });
          },
          get children() {
            return deleteIcon();
          },
        });
      },
    });
  };
  const Avatar = () => {
    createRenderEffect(() => {
      const element = avatar();
      if (isElement$1(element)) addElementClass(element, classes.avatar);
    });
    return createMemo(avatar);
  };
  const Icon = () => {
    createRenderEffect(() => {
      const element = icon();
      if (isElement$1(element)) addElementClass(element, classes.icon);
    });
    return createMemo(icon);
  };
  createEffect(() => {});
  const $ChipRoot = redefine(ChipRoot, "input", "div");
  const ownerState = {
    get color() {
      return props.color;
    },
    get disabled() {
      return props.disabled;
    },
    get size() {
      return props.size;
    },
    get variant() {
      return props.variant;
    },
    get onDelete() {
      return !!props.onDelete;
    },
    get clickable() {
      return clickable();
    },
  };
  return createComponent(
    $ChipRoot,
    mergeProps(
      {
        get as() {
          return component();
        },
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        get disabled() {
          return clickable() && props.disabled ? true : undefined;
        },
        get onClick() {
          return otherProps.onClick;
        },
        onKeyDown: handleKeyDown,
        onKeyUp: handleKeyUp,
        ref: element,
        ownerState: ownerState,
      },
      moreProps,
      otherProps,
      {
        get children() {
          return [
            createComponent(Show, {
              get when() {
                return avatar();
              },
              get fallback() {
                return createComponent(Icon, {});
              },
              get children() {
                return createComponent(Avatar, {});
              },
            }),
            createComponent(ChipLabel, {
              get ["class"]() {
                return clsx(classes.label);
              },
              ownerState: allProps,
              get children() {
                return props.label;
              },
            }),
            createComponent(DeleteIcon, {}),
          ];
        },
      },
    ),
  );
});

function getCircularProgressUtilityClass(slot) {
  return generateUtilityClass("MuiCircularProgress", slot);
}
generateUtilityClasses("MuiCircularProgress", [
  "root",
  "determinate",
  "indeterminate",
  "colorPrimary",
  "colorSecondary",
  "svg",
  "circle",
  "circleDeterminate",
  "circleIndeterminate",
  "circleDisableShrink",
]);

const $$11 = createComponentFactory()({
  name: "MuiCircularProgress",
  propDefaults: ({ set }) =>
    set({
      color: "primary",
      disableShrink: false,
      size: 40,
      thickness: 3.6,
      value: 0,
      variant: "indeterminate",
    }),
  selfPropNames: [
    "classes",
    "color",
    "disableShrink",
    "size",
    "thickness",
    "value",
    "variant",
  ],
  utilityClass: getCircularProgressUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", ownerState.variant, `color${capitalize(ownerState.color)}`],
    svg: ["svg"],
    circle: [
      "circle",
      `circle${capitalize(ownerState.variant)}`,
      ownerState.disableShrink && "circleDisableShrink",
    ],
  }),
});
const SIZE = 44;
// [review] keyframe must have a static name
const animationId$1 = randomString();
const CircularProgressRoot = styled$1("span", {
  name: "MuiCircularProgress",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[ownerState.variant],
      styles[`color${capitalize(ownerState.color)}`],
    ];
  },
})(
  ({ ownerState, theme }) => ({
    width: ownerState.size,
    height: ownerState.size,
    ...(ownerState.variant === "determinate" && {
      transform: "rotate(-90deg)",
    }),
    display: "inline-block",
    ...(ownerState.variant === "determinate" && {
      transition: theme.transitions.create("transform"),
    }),
    ...(ownerState.color !== "inherit" && {
      color: theme.palette[ownerState.color].main,
    }),
  }),
  ({ ownerState }) =>
    ownerState.variant === "indeterminate" && {
      [`@keyframes circular-rotate-${animationId$1}`]: {
        0: {
          transform: "rotate(0deg)",
        },
        100: {
          transform: "rotate(360deg)",
        },
      },
      animation: `circular-rotate-${animationId$1} 1.4s linear infinite`,
    },
);
const CircularProgressSVG = styled$1("svg", {
  name: "MuiCircularProgress",
  slot: "Svg",
  overridesResolver: (props, styles) => styles.svg,
})({
  display: "block", // Keeps the progress centered
});

const CircularProgressCircle = styled$1("circle", {
  name: "MuiCircularProgress",
  slot: "Circle",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.circle,
      styles[`circle${capitalize(ownerState.variant)}`],
      ownerState.disableShrink && styles.circleDisableShrink,
    ];
  },
})(
  ({ ownerState, theme }) => ({
    stroke: "currentColor",
    // Use butt to follow the specification, by chance, it's already the default CSS value.
    // strokeLinecap: 'butt',
    ...(ownerState.variant === "determinate" && {
      transition: theme.transitions.create("stroke-dashoffset"),
    }),
    ...(ownerState.variant === "indeterminate" && {
      // Some default value that looks fine waiting for the animation to kicks in.
      strokeDasharray: "80px, 200px",
      strokeDashoffset: 0, // Add the unit to fix a Edge 16 and below bug.
    }),
  }),
  ({ ownerState }) =>
    ownerState.variant === "indeterminate" &&
    !ownerState.disableShrink && {
      [`@keyframes circular-dash-${animationId$1}`]: {
        0: {
          strokeDasharray: "1px, 200px",
          strokeDashoffset: 0,
        },
        50: {
          strokeDasharray: "100px, 200px",
          strokeDashoffset: "-15px",
        },
        100: {
          strokeDasharray: "100px, 200px",
          strokeDashoffset: "-125px",
        },
      },
      animation: `circular-dash-${animationId$1} 1.4s ease-in-out infinite`,
    },
);

/**
 * ## ARIA
 *
 * If the progress bar is describing the loading progress of a particular region of a page,
 * you should use `aria-describedby` to point to the progress bar, and set the `aria-busy`
 * attribute to `true` on that region until it has finished loading.
 *
 * Demos:
 *
 * - [Progress](https://mui.com/components/progress/)
 *
 * API:
 *
 * - [CircularProgress API](https://mui.com/api/circular-progress/)
 */
const CircularProgress = $$11.component(function CircularProgress({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const circleStyle = () => {
    if (props.variant !== "determinate") return {};
    const circumference = 2 * Math.PI * ((SIZE - props.thickness) / 2);
    return {
      strokeDasharray: circumference.toFixed(3),
      strokeDashoffset: `${(
        ((100 - props.value) / 100) *
        circumference
      ).toFixed(3)}px`,
    };
  };
  const rootProps = () => {
    if (props.variant !== "determinate") return {};
    return {
      "aria-valuenow": Math.round(props.value),
    };
  };
  return createComponent(
    CircularProgressRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        ownerState: allProps,
        role: "progressbar",
      },
      rootProps,
      otherProps,
      {
        get children() {
          return createComponent(CircularProgressSVG, {
            get ["class"]() {
              return classes.svg;
            },
            ownerState: allProps,
            viewBox: `${SIZE / 2} ${SIZE / 2} ${SIZE} ${SIZE}`,
            get children() {
              return createComponent(CircularProgressCircle, {
                get ["class"]() {
                  return classes.circle;
                },
                get sx() {
                  return circleStyle();
                },
                ownerState: allProps,
                cx: SIZE,
                cy: SIZE,
                get r() {
                  return (SIZE - props.thickness) / 2;
                },
                fill: "none",
                get ["stroke-width"]() {
                  return props.thickness;
                },
              });
            },
          });
        },
      },
    ),
  );
});

function getContainerUtilityClass(slot) {
  return generateUtilityClass("MuiContainer", slot);
}
generateUtilityClasses("MuiContainer", [
  "root",
  "disableGutters",
  "fixed",
  "maxWidthXs",
  "maxWidthSm",
  "maxWidthMd",
  "maxWidthLg",
  "maxWidthXl",
]);

const $$10 = createComponentFactory()({
  name: "MuiContainer",
  selfPropNames: ["children", "classes", "disableGutters", "fixed", "maxWidth"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
      disableGutters: false,
      fixed: false,
      maxWidth: "lg",
    }),
  utilityClass: getContainerUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.maxWidth &&
        `maxWidth${capitalize(String(ownerState.maxWidth))}`,
      ownerState.fixed && "fixed",
      ownerState.disableGutters && "disableGutters",
    ],
  }),
});
const ContainerRoot = styled$1("div", {
  name: "MuiContainer",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[`maxWidth${capitalize(String(ownerState.maxWidth))}`],
      ownerState.fixed && styles.fixed,
      ownerState.disableGutters && styles.disableGutters,
    ];
  },
})(
  ({ theme, ownerState }) => ({
    width: "100%",
    marginLeft: "auto",
    boxSizing: "border-box",
    marginRight: "auto",
    display: "block",
    // Fix IE11 layout when used with main.
    ...(!ownerState.disableGutters && {
      paddingLeft: theme.spacing(2),
      paddingRight: theme.spacing(2),
      [theme.breakpoints.up("sm")]: {
        paddingLeft: theme.spacing(3),
        paddingRight: theme.spacing(3),
      },
    }),
  }),
  ({ theme, ownerState }) =>
    ownerState.fixed &&
    Object.keys(theme.breakpoints.values).reduce((acc, breakpoint) => {
      const value = theme.breakpoints.values[breakpoint];
      if (value !== 0) {
        acc[theme.breakpoints.up(breakpoint)] = {
          maxWidth: `${value}${theme.breakpoints.unit}`,
        };
      }
      return acc;
    }, {}),
  ({ theme, ownerState }) => ({
    ...(ownerState.maxWidth === "xs" && {
      [theme.breakpoints.up("xs")]: {
        maxWidth: Math.max(theme.breakpoints.values.xs, 444),
      },
    }),
    ...(ownerState.maxWidth &&
      ownerState.maxWidth !== "xs" && {
        [theme.breakpoints.up(ownerState.maxWidth)]: {
          maxWidth: `${theme.breakpoints.values[ownerState.maxWidth]}${
            theme.breakpoints.unit
          }`,
        },
      }),
  }),
);
/**
 *
 * Demos:
 *
 * - [Container](https://mui.com/components/container/)
 *
 * API:
 *
 * - [Container API](https://mui.com/api/container/)
 */
const Container = $$10.component(function Container({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    ContainerRoot,
    mergeProps(otherProps, {
      get as() {
        return otherProps.component;
      },
      ownerState: allProps,
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      get children() {
        return props.children;
      },
    }),
  );
});

const $$$ = createComponentFactory()({
  name: "MuiGlobalStyles",
  selfPropNames: ["styles"],
});

/**
 *
 * Demos:
 *
 * - [How To Customize](https://mui.com/customization/how-to-customize/)
 *
 * API:
 *
 * - [GlobalStyles API](https://mui.com/api/global-styles/)
 */
const GlobalStyles = $$$.component(function GlobalStyles({ props }) {
  createStyle(() => ({
    "@global": props.styles || {},
  }));
  return [];
});

const $$_ = createComponentFactory()({
  name: "MuiCssBaseline",
  selfPropNames: ["children", "enableColorScheme"],
  propDefaults: ({ set }) =>
    set({
      enableColorScheme: false,
    }),
});
const html = (theme, enableColorScheme) => ({
  WebkitFontSmoothing: "antialiased",
  // Antialiasing.
  MozOsxFontSmoothing: "grayscale",
  // Antialiasing.
  // Change from `box-sizing: content-box` so that `width`
  // is not affected by `padding` or `border`.
  boxSizing: "border-box",
  // Fix font resize problem in iOS
  WebkitTextSizeAdjust: "100%",
  ...(enableColorScheme && {
    colorScheme: theme.palette.mode,
  }),
});
const body = (theme) => ({
  color: theme.palette.text.primary,
  ...theme.typography.body1,
  backgroundColor: theme.palette.background.default,
  "@media print": {
    // Save printer ink.
    backgroundColor: theme.palette.common.white,
  },
});
const styles$2 = (theme, enableColorScheme = false) => {
  const defaultStyles = {
    html: html(theme, enableColorScheme),
    "*, *::before, *::after": {
      boxSizing: "inherit",
    },
    "strong, b": {
      fontWeight: theme.typography.fontWeightBold,
    },
    body: {
      margin: 0,
      // Remove the margin in all browsers.
      ...body(theme),
      // Add support for document.body.requestFullScreen().
      // Other elements, if background transparent, are not supported.
      "&::backdrop": {
        backgroundColor: theme.palette.background.default,
      },
    },
  };

  /*const themeOverrides = theme.components?.MuiCssBaseline?.styleOverrides;
  if (themeOverrides) {
    defaultStyles = [defaultStyles, themeOverrides];
  }*/

  return defaultStyles;
};

/**
 * Kickstart an elegant, consistent, and simple baseline to build upon.
 *
 * Demos:
 *
 * - [Css Baseline](https://mui.com/components/css-baseline/)
 *
 * API:
 *
 * - [CssBaseline API](https://mui.com/api/css-baseline/)
 */
const CssBaseline = $$_.component(function CssBaseline({ props }) {
  const theme = useTheme$1();
  return createComponent(GlobalStyles, {
    get styles() {
      return styles$2(theme, props.enableColorScheme);
    },
  });
});

/**
 * Portals provide a first-class way to render children into a DOM node
 * that exists outside the DOM hierarchy of the parent component.
 *
 * Demos:
 *
 * - [Portal](https://mui.com/components/portal/)
 *
 * API:
 *
 * - [Portal API](https://mui.com/api/portal/)
 */
function Portal(props) {
  const container = () => props.container ?? document.body;
  return createComponent(Show, {
    get when() {
      return !props.disablePortal;
    },
    get fallback() {
      return props.children;
    },
    get children() {
      return createComponent(Portal$1, {
        get mount() {
          return container();
        },
        get children() {
          return props.children;
        },
      });
    },
  });
}

// Is a vertical scrollbar displayed?
function isOverflowing(container) {
  const doc = ownerDocument(container);
  if (doc.body === container) {
    return ownerWindow(container).innerWidth > doc.documentElement.clientWidth;
  }
  return container.scrollHeight > container.clientHeight;
}
function ariaHidden(element, show) {
  if (show) {
    element.setAttribute("aria-hidden", "true");
  } else {
    element.removeAttribute("aria-hidden");
  }
}
function getPaddingRight(element) {
  return (
    parseInt(ownerWindow(element).getComputedStyle(element).paddingRight, 10) ||
    0
  );
}
function ariaHiddenSiblings(
  container,
  mountElement,
  elementsToExclude = [],
  show,
) {
  const blacklist = [mountElement, ...elementsToExclude];
  const blacklistTagNames = ["TEMPLATE", "SCRIPT", "STYLE"];
  [].forEach.call(container.children, (element) => {
    if (
      blacklist.indexOf(element) === -1 &&
      blacklistTagNames.indexOf(element.tagName) === -1
    ) {
      ariaHidden(element, show);
    }
  });
}
function findIndexOf(items, callback) {
  let idx = -1;
  items.some((item, index) => {
    if (callback(item)) {
      idx = index;
      return true;
    }
    return false;
  });
  return idx;
}
function handleContainer(containerInfo, props) {
  const restoreStyle = [];
  const container = containerInfo.container;
  if (!props.disableScrollLock) {
    if (isOverflowing(container)) {
      // Compute the size before applying overflow hidden to avoid any scroll jumps.
      const scrollbarSize = getScrollbarSize(ownerDocument(container));
      restoreStyle.push({
        value: container.style.paddingRight,
        property: "padding-right",
        el: container,
      });
      // Use computed style, here to get the real padding to add our scrollbar width.
      container.style.paddingRight = `${
        getPaddingRight(container) + scrollbarSize
      }px`;
      // .mui-fixed is a global helper.
      const fixedElements =
        ownerDocument(container).querySelectorAll(".mui-fixed");
      [].forEach.call(fixedElements, (element) => {
        restoreStyle.push({
          value: element.style.paddingRight,
          property: "padding-right",
          el: element,
        });
        element.style.paddingRight = `${
          getPaddingRight(element) + scrollbarSize
        }px`;
      });
    }
    // Improve Gatsby support
    // https://css-tricks.com/snippets/css/force-vertical-scrollbar/
    const parent = container.parentElement;
    const containerWindow = ownerWindow(container);
    const scrollContainer =
      parent?.nodeName === "HTML" &&
      containerWindow.getComputedStyle(parent).overflowY === "scroll"
        ? parent
        : container;
    // Block the scroll even if no scrollbar is visible to account for mobile keyboard
    // screensize shrink.
    restoreStyle.push(
      {
        value: scrollContainer.style.overflow,
        property: "overflow",
        el: scrollContainer,
      },
      {
        value: scrollContainer.style.overflowX,
        property: "overflow-x",
        el: scrollContainer,
      },
      {
        value: scrollContainer.style.overflowY,
        property: "overflow-y",
        el: scrollContainer,
      },
    );
    scrollContainer.style.overflow = "hidden";
  }
  const restore = () => {
    restoreStyle.forEach(({ value, el, property }) => {
      if (value) {
        el.style.setProperty(property, value);
      } else {
        el.style.removeProperty(property);
      }
    });
  };
  return restore;
}
function getHiddenSiblings(container) {
  const hiddenSiblings = [];
  [].forEach.call(container.children, (element) => {
    if (element.getAttribute("aria-hidden") === "true") {
      hiddenSiblings.push(element);
    }
  });
  return hiddenSiblings;
}
/**
 * @ignore - do not document.
 *
 * Proper state management for containers and the modals in those containers.
 * Simplified, but inspired by react-overlay's ModalManager class.
 * Used by the Modal to ensure proper styling of containers.
 */
class ModalManager {
  containers;
  modals;
  constructor() {
    this.modals = [];
    this.containers = [];
  }
  add(modal, container) {
    let modalIndex = this.modals.findIndex((v) => v.ref === modal.ref);
    if (modalIndex !== -1) {
      return modalIndex;
    }
    modalIndex = this.modals.length;
    this.modals.push(modal);
    ariaHidden(modal.ref, false);
    const hiddenSiblings = getHiddenSiblings(container);
    ariaHiddenSiblings(container, modal.ref, hiddenSiblings, true);
    const containerIndex = findIndexOf(
      this.containers,
      (item) => item.container === container,
    );
    if (containerIndex !== -1) {
      this.containers[containerIndex].modals.push(modal);
      return modalIndex;
    }
    this.containers.push({
      modals: [modal],
      container,
      restore: null,
      hiddenSiblings,
    });
    return modalIndex;
  }
  mount(modal, props) {
    const containerIndex = findIndexOf(
      this.containers,
      (item) => !!item.modals.find((v) => v.ref === modal.ref),
    );
    const containerInfo = this.containers[containerIndex];
    if (!containerInfo.restore) {
      containerInfo.restore = handleContainer(containerInfo, props);
    }
  }
  remove(modal) {
    const modalIndex = this.modals.findIndex((v) => v.ref === modal.ref);
    if (modalIndex === -1) {
      return modalIndex;
    }
    const containerIndex = findIndexOf(
      this.containers,
      (item) => !!item.modals.find((v) => v.ref === modal.ref),
    );
    const containerInfo = this.containers[containerIndex];
    containerInfo.modals.splice(
      containerInfo.modals.findIndex((v) => v.ref === modal.ref),
      1,
    );
    this.modals.splice(modalIndex, 1);
    // If that was the last modal in a container, clean up the container.
    if (containerInfo.modals.length === 0) {
      // The modal might be closed before it had the chance to be mounted in the DOM.
      if (containerInfo.restore) {
        containerInfo.restore();
      }
      ariaHidden(modal.ref, true);
      ariaHiddenSiblings(
        containerInfo.container,
        modal.ref,
        containerInfo.hiddenSiblings,
        false,
      );
      this.containers.splice(containerIndex, 1);
    } else {
      // Otherwise make sure the next top modal is visible to a screen reader.
      const nextTop = containerInfo.modals[containerInfo.modals.length - 1];
      // as soon as a modal is adding its modalRef is undefined. it can't set
      // aria-hidden because the dom element doesn't exist either
      // when modal was unmounted before modalRef gets null
      ariaHidden(nextTop.ref, false);
    }
    return modalIndex;
  }
  isTopModal(modal) {
    return (
      this.modals.length > 0 &&
      this.modals[this.modals.length - 1].ref === modal.ref
    );
  }
}

function getModalUtilityClass(slot) {
  return generateUtilityClass("MuiModal", slot);
}
generateUtilityClasses("MuiModal", ["root", "hidden"]);

const $$Z = createComponentFactory()({
  name: "ModalUnstyled",
  propDefaults: ({ set }) =>
    set({
      closeAfterTransition: false,
      component: "div",
      components: {},
      componentsProps: {},
      disableAutoFocus: false,
      disableEnforceFocus: false,
      disableEscapeKeyDown: false,
      disablePortal: false,
      disableRestoreFocus: false,
      disableScrollLock: false,
      hideBackdrop: false,
      keepMounted: false,
      open: false,
    }),
  selfPropNames: [
    "BackdropComponent",
    "BackdropProps",
    "children",
    "classes",
    "closeAfterTransition",
    "components",
    "componentsProps",
    "container",
    "disableAutoFocus",
    "disableEnforceFocus",
    "disableEscapeKeyDown",
    "disablePortal",
    "disableRestoreFocus",
    "disableScrollLock",
    "hideBackdrop",
    "keepMounted",
    "onBackdropClick",
    "onClose",
    "open",
    "transition",
  ],
  utilityClass: getModalUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", !ownerState.open && ownerState.exited && "hidden"],
  }),
});
function getContainer(container) {
  return typeof container === "function" ? container() : container;
}
// A modal manager used to track and manage the state of open Modals.
// Modals don't open on the server so this won't conflict with concurrent requests.
const defaultManager = new ModalManager();
/**
 * Modal is a lower-level construct that is leveraged by the following components:
 *
 * *   [Dialog](https://mui.com/api/dialog/)
 * *   [Drawer](https://mui.com/api/drawer/)
 * *   [Menu](https://mui.com/api/menu/)
 * *   [Popover](https://mui.com/api/popover/)
 *
 * If you are creating a modal dialog, you probably want to use the [Dialog](https://mui.com/api/dialog/) component
 * rather than directly using Modal.
 *
 * This component shares many concepts with [react-overlays](https://react-bootstrap.github.io/react-overlays/#modals).
 *
 * Demos:
 *
 * - [Modal](https://mui.com/components/modal/)
 *
 * API:
 *
 * - [ModalUnstyled API](https://mui.com/api/modal-unstyled/)
 */
const ModalUnstyled = $$Z.component(function ModalUnstyled({
  allProps,
  otherProps,
  classes,
  props,
}) {
  const element = createElementRef(otherProps);
  const manager = defaultManager;
  const getDoc = () => ownerDocument(element.ref);
  const [exited, setExited] = createSignal(true);
  const handleMounted = () => {
    manager.mount(element, {
      disableScrollLock: props.disableScrollLock,
    });
    // Fix a bug on Chrome where the scroll isn't initially 0.
    element.ref.scrollTop = 0;
  };
  const handleOpen = () => {
    const resolvedContainer = getContainer(props.container) || getDoc().body;
    manager.add(element, resolvedContainer);
    // The element was already mounted.
    if (element.ref) {
      handleMounted();
    }
  };
  const isTopModal = () => manager.isTopModal(element);
  const handleClose = () => manager.remove(element);
  onCleanup(handleClose);
  createEffect((firstTime) => {
    if (firstTime) {
      if (props.open) handleOpen();
      if (props.open && isTopModal()) {
        handleMounted();
      } else {
        if (element.ref) ariaHidden(element.ref, true);
      }
    } else {
      if (props.open) {
        handleOpen();
      } else if (!props.transition || !props.closeAfterTransition) {
        handleClose();
      }
    }
    return false;
  }, true);
  const handleBackdropClick = (event) => {
    if (event.target !== event.currentTarget) {
      return;
    }
    props.onBackdropClick?.(event);
    props.onClose?.(event, "backdropClick");
  };
  const handleKeyDown = (event) => {
    if (typeof otherProps.onKeyDown === "function")
      otherProps.onKeyDown?.(event);
    // The handler doesn't take event.defaultPrevented into account:
    //
    // event.preventDefault() is meant to stop default behaviors like
    // clicking a checkbox to check it, hitting a button to submit a form,
    // and hitting left arrow to move the cursor in a text input etc.
    // Only special HTML elements have these default behaviors.
    if (event.key !== "Escape" || !isTopModal()) {
      return;
    }
    if (!props.disableEscapeKeyDown) {
      // Swallow the event, in case someone is listening for the escape key on the body.
      event.stopPropagation();
      props.onClose?.(event, "escapeKeyDown");
    }
  };
  const Root = () => props.components.Root || otherProps.component;
  const rootProps = () => props.componentsProps.root || {};
  const noMount = () =>
    !props.keepMounted && !props.open && (!props.transition || exited());
  return createComponent(TransitionContext.Provider, {
    value: {
      get in() {
        return !!props.transition && props.open;
      },
      onEnter: () => {
        props.transition && setExited(false);
      },
      onExited: () => {
        if (props.transition) {
          setExited(true);
          if (props.closeAfterTransition) handleClose();
        }
      },
    },
    get children() {
      return createComponent(Show, {
        get when() {
          return !noMount();
        },
        get children() {
          return createComponent(Portal, {
            get container() {
              return props.container;
            },
            get disablePortal() {
              return props.disablePortal;
            },
            get children() {
              return createComponent(
                Dynamic$1,
                mergeProps(
                  otherProps,
                  {
                    get component() {
                      return Root();
                    },
                    role: "presentation",
                  },
                  rootProps,
                  () =>
                    !isHostComponent(Root()) && {
                      //component: baseProps.component,
                      ownerState: allProps,
                    },
                  {
                    onKeyDown: handleKeyDown,
                    get ["class"]() {
                      return clsx(
                        classes.root,
                        rootProps().class,
                        otherProps.class,
                      );
                    },
                    ref: element,
                    get children() {
                      return [
                        createComponent(Show, {
                          get when() {
                            return (
                              !props.hideBackdrop && !!props.BackdropComponent
                            );
                          },
                          get children() {
                            return createComponent(
                              Dynamic$1,
                              mergeProps(
                                {
                                  get component() {
                                    return props.BackdropComponent;
                                  },
                                  get open() {
                                    return props.open;
                                  },
                                  onClick: handleBackdropClick,
                                },
                                () => props.BackdropProps ?? {},
                              ),
                            );
                          },
                        }),
                        createMemo(() => props.children),
                      ];
                    },
                  },
                ),
              );
            },
          });
        },
      });
    },
  });
});

const $$Y = createComponentFactory()({
  name: "MuiModal",
  selfPropNames: ["BackdropComponent", "BackdropProps"],
});
const ModalRoot = styled$1("div", {
  name: "MuiModal",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      !ownerState.open && ownerState.exited && styles.hidden,
    ];
  },
})(({ theme, ownerState }) => ({
  position: "fixed",
  zIndex: theme.zIndex.modal,
  right: 0,
  bottom: 0,
  top: 0,
  left: 0,
  ...(!ownerState.open &&
    ownerState.exited && {
      visibility: "hidden",
    }),
}));
const ModalBackdrop = styled$1(Backdrop, {
  name: "MuiModal",
  slot: "Backdrop",
  overridesResolver: (props, styles) => {
    return styles.backdrop;
  },
})({
  zIndex: -1,
});

/**
 * Modal is a lower-level construct that is leveraged by the following components:
 *
 * *   [Dialog](https://mui.com/api/dialog/)
 * *   [Drawer](https://mui.com/api/drawer/)
 * *   [Menu](https://mui.com/api/menu/)
 * *   [Popover](https://mui.com/api/popover/)
 *
 * If you are creating a modal dialog, you probably want to use the [Dialog](https://mui.com/api/dialog/) component
 * rather than directly using Modal.
 *
 * This component shares many concepts with [react-overlays](https://react-bootstrap.github.io/react-overlays/#modals).
 *
 * Demos:
 *
 * - [Modal](https://mui.com/components/modal/)
 *
 * API:
 *
 * - [Modal API](https://mui.com/api/modal/)
 */
const Modal = $$Y.defineComponent(function Modal(inProps) {
  const props = $$Y.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "BackdropComponent",
    "closeAfterTransition",
    "children",
    "components",
    "componentsProps",
    "disableAutoFocus",
    "disableEnforceFocus",
    "disableEscapeKeyDown",
    "disablePortal",
    "disableRestoreFocus",
    "disableScrollLock",
    "hideBackdrop",
    "keepMounted",
  ]);
  const baseProps = mergeProps(
    {
      BackdropComponent: ModalBackdrop,
      closeAfterTransition: false,
      components: {},
      componentsProps: {},
      disableAutoFocus: false,
      disableEnforceFocus: false,
      disableEscapeKeyDown: false,
      disablePortal: false,
      disableRestoreFocus: false,
      disableScrollLock: false,
      hideBackdrop: false,
      keepMounted: false,
    },
    props,
  );
  const [exited] = createSignal(true);
  const commonProps = {
    get closeAfterTransition() {
      return baseProps.closeAfterTransition;
    },
    get disableAutoFocus() {
      return baseProps.disableAutoFocus;
    },
    get disableEnforceFocus() {
      return baseProps.disableEnforceFocus;
    },
    get disableEscapeKeyDown() {
      return baseProps.disableEscapeKeyDown;
    },
    get disablePortal() {
      return baseProps.disablePortal;
    },
    get disableRestoreFocus() {
      return baseProps.disableRestoreFocus;
    },
    get disableScrollLock() {
      return baseProps.disableScrollLock;
    },
    get hideBackdrop() {
      return baseProps.hideBackdrop;
    },
    get keepMounted() {
      return baseProps.keepMounted;
    },
  };
  const ownerState = mergeProps(props, commonProps, {
    get exited() {
      return exited();
    },
  });
  return createComponent(
    ModalUnstyled,
    mergeProps(
      {
        get components() {
          return mergeProps(
            {
              Root: ModalRoot,
            },
            () => baseProps.components,
          );
        },
        componentsProps: {
          get root() {
            return mergeProps(
              () => baseProps.componentsProps.root || {},
              () =>
                ((!baseProps.components.Root ||
                  !isHostComponent(baseProps.components.Root)) && {
                  get ownerState() {
                    return baseProps.componentsProps.root?.ownerState || {};
                  },
                }) ||
                {},
            );
          },
        },
        get BackdropComponent() {
          return baseProps.BackdropComponent;
        },
      },
      other,
      {
        get classes() {
          return ownerState.classes;
        },
      },
      commonProps,
      {
        get children() {
          return props.children;
        },
      },
    ),
  );
});

const DialogContext = createContext({});

function getDialogUtilityClass(slot) {
  return generateUtilityClass("MuiDialog", slot);
}
const dialogClasses = generateUtilityClasses("MuiDialog", [
  "root",
  "scrollPaper",
  "scrollBody",
  "container",
  "paper",
  "paperScrollPaper",
  "paperScrollBody",
  "paperWidthFalse",
  "paperWidthXs",
  "paperWidthSm",
  "paperWidthMd",
  "paperWidthLg",
  "paperWidthXl",
  "paperFullWidth",
  "paperFullScreen",
]);

const $$X = createComponentFactory()({
  name: "MuiDialog",
  selfPropNames: [
    "aria-describedby",
    "aria-labelledby",
    "children",
    "classes",
    "disableEscapeKeyDown",
    "fullScreen",
    "fullWidth",
    "maxWidth",
    "onBackdropClick",
    "onClose",
    "open",
    "PaperComponent",
    "PaperProps",
    "scroll",
    "TransitionComponent",
    "transitionDuration",
    "TransitionProps",
  ],
  utilityClass: getDialogUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root"],
    container: ["container", `scroll${capitalize(ownerState.scroll)}`],
    paper: [
      "paper",
      `paperScroll${capitalize(ownerState.scroll)}`,
      `paperWidth${capitalize(String(ownerState.maxWidth))}`,
      ownerState.fullWidth && "paperFullWidth",
      ownerState.fullScreen && "paperFullScreen",
    ],
  }),
});
const DialogBackdrop = styled$1(Backdrop, {
  name: "MuiDialog",
  slot: "Backdrop",
  //overrides: (props, styles) => styles.backdrop,
})({
  // Improve scrollable dialog support.
  zIndex: -1,
});
const DialogRoot = styled$1(Modal, {
  name: "MuiDialog",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  "@media print": {
    // Use !important to override the Modal inline-style.
    position: "absolute !important",
  },
});
const DialogContainer = styled$1("div", {
  name: "MuiDialog",
  slot: "Container",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.container, styles[`scroll${capitalize(ownerState.scroll)}`]];
  },
})(({ ownerState }) => ({
  height: "100%",
  "@media print": {
    height: "auto",
  },
  // We disable the focus ring for mouse, touch and keyboard users.
  outline: 0,
  ...(ownerState.scroll === "paper" && {
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
  }),
  ...(ownerState.scroll === "body" && {
    overflowY: "auto",
    overflowX: "hidden",
    textAlign: "center",
    "&:after": {
      content: '""',
      display: "inline-block",
      verticalAlign: "middle",
      height: "100%",
      width: "0",
    },
  }),
}));
const DialogPaper = styled$1(Paper, {
  name: "MuiDialog",
  slot: "Paper",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.paper,
      styles[`scrollPaper${capitalize(ownerState.scroll)}`],
      styles[`paperWidth${capitalize(String(ownerState.maxWidth))}`],
      ownerState.fullWidth && styles.paperFullWidth,
      ownerState.fullScreen && styles.paperFullScreen,
    ];
  },
})(({ theme, ownerState }) => ({
  margin: 32,
  position: "relative",
  overflowY: "auto",
  // Fix IE11 issue, to remove at some point.
  "@media print": {
    overflowY: "visible",
    boxShadow: "none",
  },
  ...(ownerState.scroll === "paper" && {
    display: "flex",
    flexDirection: "column",
    maxHeight: "calc(100% - 64px)",
  }),
  ...(ownerState.scroll === "body" && {
    display: "inline-block",
    verticalAlign: "middle",
    textAlign: "left", // 'initial' doesn't work on IE11
  }),

  ...(!ownerState.maxWidth && {
    maxWidth: "calc(100% - 64px)",
  }),
  ...(ownerState.maxWidth === "xs" && {
    maxWidth:
      theme.breakpoints.unit === "px"
        ? Math.max(theme.breakpoints.values.xs, 444)
        : `${theme.breakpoints.values.xs}${theme.breakpoints.unit}`,
    [`&.${dialogClasses.paperScrollBody}`]: {
      [theme.breakpoints.down(
        Math.max(theme.breakpoints.values.xs, 444) + 32 * 2,
      )]: {
        maxWidth: "calc(100% - 64px)",
      },
    },
  }),
  ...(ownerState.maxWidth !== "xs" && {
    maxWidth: `${theme.breakpoints.values[ownerState.maxWidth]}${
      theme.breakpoints.unit
    }`,
    [`&.${dialogClasses.paperScrollBody}`]: {
      [theme.breakpoints.down(
        theme.breakpoints.values[ownerState.maxWidth] + 32 * 2,
      )]: {
        maxWidth: "calc(100% - 64px)",
      },
    },
  }),
  ...(ownerState.fullWidth && {
    width: "calc(100% - 64px)",
  }),
  ...(ownerState.fullScreen && {
    margin: 0,
    width: "100%",
    maxWidth: "100%",
    height: "100%",
    maxHeight: "none",
    borderRadius: 0,
    [`&.${dialogClasses.paperScrollBody}`]: {
      margin: 0,
      maxWidth: "100%",
    },
  }),
}));

/**
 * Dialogs are overlaid modal paper based components with a backdrop.
 *
 * Demos:
 *
 * - [Dialogs](https://mui.com/components/dialogs/)
 *
 * API:
 *
 * - [Dialog API](https://mui.com/api/dialog/)
 * - inherits [Modal API](https://mui.com/api/modal/)
 */
const Dialog = $$X.defineComponent(function Dialog(inProps) {
  const ref = createRef(inProps);
  const props = $$X.useThemeProps({
    props: inProps,
  });
  const theme = useTheme$1();
  const defaultTransitionDuration = {
    get enter() {
      return theme.transitions.duration.enteringScreen;
    },
    get exit() {
      return theme.transitions.duration.leavingScreen;
    },
  };
  const [, other] = splitProps(props, [
    "aria-describedby",
    "aria-labelledby",
    "BackdropComponent",
    "BackdropProps",
    "children",
    "class",
    "disableEscapeKeyDown",
    "fullScreen",
    "fullWidth",
    "maxWidth",
    "onBackdropClick",
    "onClose",
    "open",
    "PaperComponent",
    "PaperProps",
    "scroll",
    "TransitionComponent",
    "transitionDuration",
    "TransitionProps",
  ]);
  const baseProps = mergeProps(
    {
      disableEscapeKeyDown: false,
      fullScreen: false,
      fullWidth: false,
      maxWidth: "sm",
      PaperComponent: Paper,
      PaperProps: {},
      scroll: "paper",
      TransitionComponent: Fade,
      transitionDuration: defaultTransitionDuration,
    },
    props,
  );
  const ownerState = baseProps;
  const classes = $$X.useClasses(ownerState);
  let backdropClick = null;
  const handleMouseDown = (event) => {
    // We don't want to close the dialog when clicking the dialog content.
    // Make sure the event starts and ends on the same DOM element.
    backdropClick = event.target === event.currentTarget;
  };
  const handleBackdropClick = (event) => {
    // Ignore the events not coming from the "backdrop".
    if (!backdropClick) {
      return;
    }
    backdropClick = null;
    if (props.onBackdropClick) {
      props.onBackdropClick(event);
    }
    if (props.onClose) {
      props.onClose(event, "backdropClick");
    }
  };
  const ariaLabelledby = createUniqueId(() => props["aria-labelledby"]);
  const dialogContextValue = {
    get titleId() {
      return ariaLabelledby();
    },
  };
  return createComponent(
    DialogRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
        get BackdropProps() {
          return mergeProps(
            {
              get transitionDuration() {
                return baseProps.transitionDuration;
              },
              get as() {
                return props.BackdropComponent;
              },
            },
            () => props.BackdropProps,
          );
        },
        closeAfterTransition: true,
        BackdropComponent: DialogBackdrop,
        get disableEscapeKeyDown() {
          return baseProps.disableEscapeKeyDown;
        },
        get onClose() {
          return props.onClose;
        },
        get open() {
          return props.open;
        },
        ref: ref,
        onClick: handleBackdropClick,
        ownerState: ownerState,
      },
      other,
      {
        get children() {
          return createComponent(
            baseProps.TransitionComponent,
            mergeProps(
              {
                appear: true,
                get ["in"]() {
                  return props.open;
                },
                get timeout() {
                  return baseProps.transitionDuration;
                },
              },
              () => props.TransitionProps,
              {
                get children() {
                  return createComponent(DialogContainer, {
                    get ["class"]() {
                      return clsx(classes.container);
                    },
                    onMouseDown: handleMouseDown,
                    ownerState: ownerState,
                    get children() {
                      return createComponent(
                        DialogPaper,
                        mergeProps(
                          {
                            get component() {
                              return baseProps.PaperComponent;
                            },
                            elevation: 24,
                            role: "dialog",
                            get ["aria-describedby"]() {
                              return props["aria-describedby"];
                            },
                            get ["aria-labelledby"]() {
                              return ariaLabelledby();
                            },
                          },
                          () => baseProps.PaperProps,
                          {
                            get ["class"]() {
                              return clsx(
                                classes.paper,
                                baseProps.PaperProps.class,
                              );
                            },
                            ownerState: ownerState,
                            get children() {
                              return createComponent(DialogContext.Provider, {
                                value: dialogContextValue,
                                get children() {
                                  return props.children;
                                },
                              });
                            },
                          },
                        ),
                      );
                    },
                  });
                },
              },
            ),
          );
        },
      },
    ),
  );
});

function getDialogActionsUtilityClass(slot) {
  return generateUtilityClass("MuiDialogActions", slot);
}
generateUtilityClasses("MuiDialogActions", ["root", "spacing"]);

const $$W = createComponentFactory()({
  name: "MuiDialogActions",
  selfPropNames: ["children", "classes", "disableSpacing"],
  utilityClass: getDialogActionsUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", !ownerState.disableSpacing && "spacing"],
  }),
});
const DialogActionsRoot = styled$1("div", {
  name: "MuiDialogActions",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.root, !ownerState.disableSpacing && styles.spacing];
  },
})(({ ownerState }) => ({
  display: "flex",
  alignItems: "center",
  padding: 8,
  justifyContent: "flex-end",
  flex: "0 0 auto",
  ...(!ownerState.disableSpacing && {
    "& > :not(:first-of-type)": {
      marginLeft: 8,
    },
  }),
}));

/**
 *
 * Demos:
 *
 * - [Dialogs](https://mui.com/components/dialogs/)
 *
 * API:
 *
 * - [DialogActions API](https://mui.com/api/dialog-actions/)
 */
const DialogActions = $$W.defineComponent(function DialogActions(inProps) {
  const props = $$W.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, ["class", "disableSpacing"]);
  const baseProps = mergeProps(
    {
      disableSpacing: false,
    },
    props,
  );
  const ownerState = baseProps;
  const classes = $$W.useClasses(ownerState);
  return createComponent(
    DialogActionsRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
        ownerState: ownerState,
      },
      other,
    ),
  );
});

function getDialogTitleUtilityClass(slot) {
  return generateUtilityClass("MuiDialogTitle", slot);
}
const dialogTitleClasses = generateUtilityClasses("MuiDialogTitle", ["root"]);

function getDialogContentUtilityClass(slot) {
  return generateUtilityClass("MuiDialogContent", slot);
}
generateUtilityClasses("MuiDialogContent", ["root", "dividers"]);

const $$V = createComponentFactory()({
  name: "MuiDialogContent",
  selfPropNames: ["children", "classes", "dividers"],
  utilityClass: getDialogContentUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", ownerState.dividers && "dividers"],
  }),
});
const DialogContentRoot = styled$1("div", {
  name: "MuiDialogContent",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.root, ownerState.dividers && styles.dividers];
  },
})(({ theme, ownerState }) => ({
  flex: "1 1 auto",
  // Add iOS momentum scrolling for iOS < 13.0
  WebkitOverflowScrolling: "touch",
  overflowY: "auto",
  // https://github.com/microsoft/TypeScript/issues/37559
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  padding: "20px 24px",
  ...(ownerState.dividers
    ? {
        padding: "16px 24px",
        borderTop: `1px solid ${theme.palette.divider}`,
        borderBottom: `1px solid ${theme.palette.divider}`,
      }
    : {
        [`.${dialogTitleClasses.root} + &`]: {
          paddingTop: 0,
        },
      }),
}));

/**
 *
 * Demos:
 *
 * - [Dialogs](https://mui.com/components/dialogs/)
 *
 * API:
 *
 * - [DialogContent API](https://mui.com/api/dialog-content/)
 */
const DialogContent = $$V.defineComponent(function DialogContent(inProps) {
  const props = $$V.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, ["class", "dividers"]);
  const baseProps = mergeProps(
    {
      dividers: false,
    },
    props,
  );
  const ownerState = mergeProps(props, {
    get dividers() {
      return baseProps.dividers;
    },
  });
  const classes = $$V.useClasses(ownerState);
  return createComponent(
    DialogContentRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
        ownerState: ownerState,
      },
      other,
    ),
  );
});

function getDialogContentTextUtilityClass(slot) {
  return generateUtilityClass("MuiDialogContentText", slot);
}
generateUtilityClasses("MuiDialogContentText", ["root"]);

const $$U = createComponentFactory()({
  name: "MuiDialogContentText",
  selfPropNames: ["classes"],
  utilityClass: getDialogContentTextUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const DialogContentTextRoot = styled$1(Typography, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiDialogContentText",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({});

/**
 *
 * Demos:
 *
 * - [Dialogs](https://mui.com/components/dialogs/)
 *
 * API:
 *
 * - [DialogContentText API](https://mui.com/api/dialog-content-text/)
 * - inherits [Typography API](https://mui.com/api/typography/)
 */
const DialogContentText = $$U.defineComponent(
  function DialogContentText(inProps) {
    const ref = createRef(inProps);
    const props = $$U.useThemeProps({
      props: inProps,
    });
    const [, ownerState] = splitProps(props, ["children"]);
    const classes = $$U.useClasses(ownerState);
    return createComponent(
      DialogContentTextRoot,
      mergeProps(
        {
          variant: "body1",
          color: "text.secondary",
          ref: ref,
          ownerState: ownerState,
        },
        props,
        {
          get component() {
            return props.component ?? "p";
          },
          classes: classes,
        },
      ),
    );
  },
);

const $$T = createComponentFactory()({
  name: "MuiDialogTitle",
  selfPropNames: ["children", "classes"],
  utilityClass: getDialogTitleUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const DialogTitleRoot = styled$1(Typography, {
  name: "MuiDialogTitle",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  padding: "16px 24px",
  flex: "0 0 auto",
});

/**
 *
 * Demos:
 *
 * - [Dialogs](https://mui.com/components/dialogs/)
 *
 * API:
 *
 * - [DialogTitle API](https://mui.com/api/dialog-title/)
 */
const DialogTitle = $$T.defineComponent(function DialogTitle(inProps) {
  const props = $$T.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, ["class", "id"]);
  const ownerState = props;
  const classes = $$T.useClasses(ownerState);
  const context = useContext(DialogContext);
  const titleId = () => context.titleId ?? props.id;
  return createComponent(
    DialogTitleRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
        ownerState: ownerState,
        variant: "h6",
        get id() {
          return titleId();
        },
      },
      other,
      {
        get component() {
          return props.component ?? "h2";
        },
      },
    ),
  );
});

function getDividerUtilityClass(slot) {
  return generateUtilityClass("MuiDivider", slot);
}
const dividerClasses = generateUtilityClasses("MuiDivider", [
  "root",
  "absolute",
  "fullWidth",
  "inset",
  "middle",
  "flexItem",
  "light",
  "vertical",
  "withChildren",
  "withChildrenVertical",
  "textAlignRight",
  "textAlignLeft",
  "wrapper",
  "wrapperVertical",
]);

const $$S = createComponentFactory()({
  name: "MuiDivider",
  selfPropNames: [
    "absolute",
    "children",
    "classes",
    "flexItem",
    "light",
    "orientation",
    "textAlign",
    "variant",
  ],
  propDefaults: ({ set, inProps }) =>
    set({
      absolute: false,
      get component() {
        return inProps.children ? "div" : "hr";
      },
      flexItem: false,
      light: false,
      orientation: "horizontal",
      get role() {
        return inProps.component !== "hr" ? "separator" : undefined;
      },
      textAlign: "center",
      variant: "fullWidth",
    }),
  utilityClass: getDividerUtilityClass,
  slotClasses: (o) => ({
    root: [
      "root",
      o.absolute && "absolute",
      o.variant,
      o.light && "light",
      o.orientation === "vertical" && "vertical",
      o.flexItem && "flexItem",
      !!o.children && "withChildren",
      !!o.children && o.orientation === "vertical" && "withChildrenVertical",
      o.textAlign === "right" &&
        o.orientation !== "vertical" &&
        "textAlignRight",
      o.textAlign === "left" && o.orientation !== "vertical" && "textAlignLeft",
    ],
    wrapper: ["wrapper", o.orientation === "vertical" && "wrapperVertical"],
  }),
});
const DividerRoot = styled$1("div", {
  name: "MuiDivider",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.absolute && styles.absolute,
      styles[ownerState.variant],
      ownerState.light && styles.light,
      ownerState.orientation === "vertical" && styles.vertical,
      ownerState.flexItem && styles.flexItem,
      ownerState.children && styles.withChildren,
      ownerState.children &&
        ownerState.orientation === "vertical" &&
        styles.withChildrenVertical,
      ownerState.textAlign === "right" &&
        ownerState.orientation !== "vertical" &&
        styles.textAlignRight,
      ownerState.textAlign === "left" &&
        ownerState.orientation !== "vertical" &&
        styles.textAlignLeft,
    ];
  },
})(
  ({ theme, ownerState }) => ({
    margin: 0,
    // Reset browser default style.
    flexShrink: 0,
    borderWidth: 0,
    borderStyle: "solid",
    borderColor: theme.palette.divider,
    borderBottomWidth: "thin",
    ...(ownerState.absolute && {
      position: "absolute",
      bottom: 0,
      left: 0,
      width: "100%",
    }),
    ...(ownerState.light && {
      borderColor: alpha(theme.palette.divider, 0.08),
    }),
    ...(ownerState.variant === "inset" && {
      marginLeft: 72,
    }),
    ...(ownerState.variant === "middle" &&
      ownerState.orientation === "horizontal" && {
        marginLeft: theme.spacing(2),
        marginRight: theme.spacing(2),
      }),
    ...(ownerState.variant === "middle" &&
      ownerState.orientation === "vertical" && {
        marginTop: theme.spacing(1),
        marginBottom: theme.spacing(1),
      }),
    ...(ownerState.orientation === "vertical" && {
      height: "100%",
      borderBottomWidth: 0,
      borderRightWidth: "thin",
    }),
    ...(ownerState.flexItem && {
      alignSelf: "stretch",
      height: "auto",
    }),
  }),
  ({ theme, ownerState }) => ({
    ...(ownerState.children && {
      display: "flex",
      whiteSpace: "nowrap",
      textAlign: "center",
      border: 0,
      "&::before, &::after": {
        position: "relative",
        width: "100%",
        borderTop: `thin solid ${theme.palette.divider}`,
        top: "50%",
        content: '""',
        transform: "translateY(50%)",
      },
    }),
  }),
  ({ theme, ownerState }) => ({
    ...(ownerState.children &&
      ownerState.orientation === "vertical" && {
        flexDirection: "column",
        "&::before, &::after": {
          height: "100%",
          top: "0%",
          left: "50%",
          borderTop: 0,
          borderLeft: `thin solid ${theme.palette.divider}`,
          transform: "translateX(0%)",
        },
      }),
  }),
  ({ ownerState }) => ({
    ...(ownerState.textAlign === "right" &&
      ownerState.orientation !== "vertical" && {
        "&::before": {
          width: "90%",
        },
        "&::after": {
          width: "10%",
        },
      }),
    ...(ownerState.textAlign === "left" &&
      ownerState.orientation !== "vertical" && {
        "&::before": {
          width: "10%",
        },
        "&::after": {
          width: "90%",
        },
      }),
  }),
);
const DividerWrapper = styled$1("span", {
  name: "MuiDivider",
  slot: "Wrapper",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.wrapper,
      ownerState.orientation === "vertical" && styles.wrapperVertical,
    ];
  },
})(({ theme, ownerState }) => ({
  display: "inline-block",
  paddingLeft: `calc(${theme.spacing(1)} * 1.2)`,
  paddingRight: `calc(${theme.spacing(1)} * 1.2)`,
  ...(ownerState.orientation === "vertical" && {
    paddingTop: `calc(${theme.spacing(1)} * 1.2)`,
    paddingBottom: `calc(${theme.spacing(1)} * 1.2)`,
  }),
}));

/**
 *
 * Demos:
 *
 * - [Dividers](https://mui.com/components/dividers/)
 * - [Lists](https://mui.com/components/lists/)
 *
 * API:
 *
 * - [Divider API](https://mui.com/api/divider/)
 */

const Divider = $$S.component(function Divider({
  allProps,
  otherProps,
  classes,
}) {
  const $DividerRoot = redefine(DividerRoot, "div", "hr");
  return createComponent(
    $DividerRoot,
    mergeProps(
      {
        get role() {
          return allProps.role;
        },
        get as() {
          return otherProps.component;
        },
      },
      otherProps,
      {
        ownerState: allProps,
        get ["class"]() {
          return clsx(classes.root, allProps.class);
        },
        get children() {
          return createComponent(Show, {
            get when() {
              return allProps.children;
            },
            children: (children) =>
              createComponent(DividerWrapper, {
                get ["class"]() {
                  return classes.wrapper;
                },
                ownerState: allProps,
                get children() {
                  return children();
                },
              }),
          });
        },
      },
    ),
  );
});

const $$R = createComponentFactory()({
  name: "MuiSlide",
  selfPropNames: [
    "appear",
    "children",
    "container",
    "direction",
    "easing",
    "in",
    "timeout",
  ],
  propDefaults: ({ set }) => {
    const theme = useTheme$1();
    return set({
      appear: true,
      direction: "down",
      get easing() {
        return {
          enter: theme.transitions.easing.easeOut,
          exit: theme.transitions.easing.sharp,
        };
      },
      get timeout() {
        return {
          enter: theme.transitions.duration.enteringScreen,
          exit: theme.transitions.duration.leavingScreen,
        };
      },
    });
  },
});
// Translate the node so it can't be seen on the screen.
// Later, we're going to translate the node back to its original location with `none`.
function getTranslateValue(direction, node, resolvedContainer) {
  const rect = node.getBoundingClientRect();
  const containerRect =
    resolvedContainer && resolvedContainer.getBoundingClientRect();
  const containerWindow = ownerWindow(node);
  const computedStyle = containerWindow.getComputedStyle(node);
  const transform =
    computedStyle.getPropertyValue("-webkit-transform") ||
    computedStyle.getPropertyValue("transform");
  let offsetX = 0;
  let offsetY = 0;
  if (transform && transform !== "none" && typeof transform === "string") {
    const transformValues = transform.split("(")[1].split(")")[0].split(",");
    offsetX = parseInt(transformValues[4], 10);
    offsetY = parseInt(transformValues[5], 10);
  }
  if (direction === "left") {
    if (containerRect) {
      return `translateX(${containerRect.right + offsetX - rect.left}px)`;
    }
    return `translateX(${containerWindow.innerWidth + offsetX - rect.left}px)`;
  }
  if (direction === "right") {
    if (containerRect) {
      return `translateX(-${rect.right - containerRect.left - offsetX}px)`;
    }
    return `translateX(-${rect.left + rect.width - offsetX}px)`;
  }
  if (direction === "up") {
    if (containerRect) {
      return `translateY(${containerRect.bottom + offsetY - rect.top}px)`;
    }
    return `translateY(${containerWindow.innerHeight + offsetY - rect.top}px)`;
  }

  // direction === 'down'
  if (containerRect) {
    return `translateY(-${
      rect.top - containerRect.top + rect.height - offsetY
    }px)`;
  }
  return `translateY(-${rect.top + rect.height - offsetY}px)`;
}
function resolveContainer(containerPropProp) {
  return typeof containerPropProp === "function"
    ? containerPropProp()
    : containerPropProp;
}
function setTranslateValue(direction, node, containerProp) {
  const resolvedContainer = resolveContainer(containerProp);
  const transform = getTranslateValue(direction, node, resolvedContainer);
  if (transform) {
    node.style.webkitTransform = transform;
    node.style.transform = transform;
  }
}

/**
 * The Slide transition is used by the [Drawer](https://mui.com/components/drawers/) component.
 * It uses [react-transition-group](https://github.com/reactjs/react-transition-group) internally.
 *
 * Demos:
 *
 * - [Dialogs](https://mui.com/components/dialogs/)
 * - [Transitions](https://mui.com/components/transitions/)
 *
 * API:
 *
 * - [Slide API](https://mui.com/api/slide/)
 * - inherits [Transition API](http://reactcommunity.org/react-transition-group/transition/#Transition-props)
 */

const Slide = $$R.component(function Slide({ props, otherProps }) {
  const theme = useTheme$1();
  const resolved = children(() => props.children);
  const context = useContext(TransitionContext);
  let destructors = [];
  createEffect((prev) => {
    if (prev) {
      destructors = destructors.filter((v) => v !== prev);
      prev();
    }
    if (!props.in)
      setTranslateValue(props.direction, resolved(), props.container);

    // Skip configuration where the position is screen size invariant.
    if (props.in || props.direction === "down" || props.direction === "right") {
      return undefined;
    }
    const handleResize = debounce$1(() => {
      setTranslateValue(props.direction, resolved(), props.container);
    });
    const containerWindow = ownerWindow(resolved());
    containerWindow.addEventListener("resize", handleResize);
    return () => {
      handleResize.clear();
      containerWindow.removeEventListener("resize", handleResize);
    };
  });
  onCleanup(() => {
    for (const destructor of destructors) destructor();
  });
  return createComponent(
    Transition,
    mergeProps(
      {
        get ["in"]() {
          return props.in;
        },
        get appear() {
          return props.appear;
        },
        get timeout() {
          return props.timeout;
        },
      },
      otherProps,
      {
        onEnter: () => {
          const node = resolved();
          setTranslateValue(props.direction, node, props.container);
          reflow(node);
          otherProps.onEnter?.();
          context?.onEnter?.();
        },
        onEntering: () => {
          const node = resolved();
          const transitionProps = getTransitionProps(
            {
              timeout: props.timeout,
              style: {},
              easing: props.easing,
            },
            {
              mode: "enter",
            },
          );
          node.style.webkitTransition = theme.transitions.create(
            "-webkit-transform",
            {
              ...transitionProps,
            },
          );
          node.style.transition = theme.transitions.create("transform", {
            ...transitionProps,
          });
          node.style.webkitTransform = "none";
          node.style.transform = "none";
        },
        onExit: () => {
          const node = resolved();
          const transitionProps = getTransitionProps(
            {
              timeout: props.timeout,
              style: {},
              easing: props.easing,
            },
            {
              mode: "exit",
            },
          );
          node.style.webkitTransition = theme.transitions.create(
            "-webkit-transform",
            transitionProps,
          );
          node.style.transition = theme.transitions.create(
            "transform",
            transitionProps,
          );
          setTranslateValue(props.direction, node, props.container);
          otherProps.onExit?.();
        },
        onExited: () => {
          const node = resolved();
          // No need for transitions when the component is hidden
          node.style.webkitTransition = "";
          node.style.transition = "";
          otherProps.onExited?.();
          context?.onExited?.();
        },
        children: (state) => {
          const element = resolved();
          if (state === "exited" && !props.in) {
            element.style.visibility = "hidden";
          } else {
            element.style.removeProperty("visibility");
          }
          return element;
        },
      },
    ),
  );
});

function getDrawerUtilityClass(slot) {
  return generateUtilityClass("MuiDrawer", slot);
}
generateUtilityClasses("MuiDrawer", [
  "root",
  "docked",
  "paper",
  "paperAnchorLeft",
  "paperAnchorRight",
  "paperAnchorTop",
  "paperAnchorBottom",
  "paperAnchorDockedLeft",
  "paperAnchorDockedRight",
  "paperAnchorDockedTop",
  "paperAnchorDockedBottom",
  "modal",
]);

const $$Q = createComponentFactory()({
  name: "MuiDrawer",
  selfPropNames: [
    "ModalProps",
    "PaperProps",
    "anchor",
    "children",
    "classes",
    "elevation",
    "onClose",
    "open",
    "transitionDuration",
    "variant",
  ],
  utilityClass: getDrawerUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root"],
    docked: [
      (ownerState.variant === "permanent" ||
        ownerState.variant === "persistent") &&
        "docked",
    ],
    modal: ["modal"],
    paper: [
      "paper",
      `paperAnchor${capitalize(ownerState.anchor)}`,
      ownerState.variant !== "temporary" &&
        `paperAnchorDocked${capitalize(ownerState.anchor)}`,
    ],
  }),
  propDefaults: ({ set }) => {
    const theme = useTheme$1();
    return set({
      component: "div",
      anchor: "left",
      elevation: 16,
      hideBackdrop: false,
      ModalProps: {},
      open: false,
      PaperProps: {},
      get transitionDuration() {
        return {
          enter: theme.transitions.duration.enteringScreen,
          exit: theme.transitions.duration.leavingScreen,
        };
      },
      variant: "temporary",
    });
  },
});
const DrawerRoot = styled$1(Modal, {
  name: "MuiDrawer",
  slot: "Root",
})(({ theme }) => ({
  zIndex: theme.zIndex.drawer,
}));
const DrawerDockedRoot = styled$1("div", {
  skipProps: skipRootProps,
  name: "MuiDrawer",
  slot: "Docked",
  //skipVariantsResolver: false,
})({
  flex: "0 0 auto",
});
const DrawerPaper = styled$1(Paper, {
  name: "MuiDrawer",
  slot: "Paper",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.paper,
      styles[`paperAnchor${capitalize(ownerState.anchor)}`],
      ownerState.variant !== "temporary" &&
        styles[`paperAnchorDocked${capitalize(ownerState.anchor)}`],
    ];
  },
})(({ theme, ownerState }) => ({
  overflowY: "auto",
  display: "flex",
  flexDirection: "column",
  height: "100%",
  flex: "1 0 auto",
  zIndex: theme.zIndex.drawer,
  // Add iOS momentum scrolling for iOS < 13.0
  WebkitOverflowScrolling: "touch",
  // temporary style
  position: "fixed",
  top: 0,
  // We disable the focus ring for mouse, touch and keyboard users.
  // At some point, it would be better to keep it for keyboard users.
  // :focus-ring CSS pseudo-class will help.
  outline: 0,
  ...(ownerState.anchor === "left" && {
    left: 0,
  }),
  ...(ownerState.anchor === "top" && {
    top: 0,
    left: 0,
    right: 0,
    height: "auto",
    maxHeight: "100%",
  }),
  ...(ownerState.anchor === "right" && {
    right: 0,
  }),
  ...(ownerState.anchor === "bottom" && {
    top: "auto",
    left: 0,
    bottom: 0,
    right: 0,
    height: "auto",
    maxHeight: "100%",
  }),
  ...(ownerState.anchor === "left" &&
    ownerState.variant !== "temporary" && {
      borderRight: `1px solid ${theme.palette.divider}`,
    }),
  ...(ownerState.anchor === "top" &&
    ownerState.variant !== "temporary" && {
      borderBottom: `1px solid ${theme.palette.divider}`,
    }),
  ...(ownerState.anchor === "right" &&
    ownerState.variant !== "temporary" && {
      borderLeft: `1px solid ${theme.palette.divider}`,
    }),
  ...(ownerState.anchor === "bottom" &&
    ownerState.variant !== "temporary" && {
      borderTop: `1px solid ${theme.palette.divider}`,
    }),
}));
const oppositeDirection = {
  left: "right",
  right: "left",
  top: "down",
  bottom: "up",
};
function isHorizontal(anchor) {
  return ["left", "right"].indexOf(anchor) !== -1;
}
function getAnchor(theme, anchor) {
  return theme.direction === "rtl" && isHorizontal(anchor)
    ? oppositeDirection[anchor]
    : anchor;
}

/**
 * The props of the [Modal](https://mui.com/api/modal/) component are available
 * when `variant="temporary"` is set.
 *
 * Demos:
 *
 * - [Drawers](https://mui.com/components/drawers/)
 *
 * API:
 *
 * - [Drawer API](https://mui.com/api/drawer/)
 */
const Drawer = $$Q.component(function Drawer({
  allProps,
  props,
  otherProps,
  classes,
}) {
  // Let's assume that the Drawer will always be rendered on user space.
  // We use this state is order to skip the appear transition during the
  // initial mount of the component.

  const [mounted, setMounted] = createSignal(false);
  const element = createElementRef(otherProps);
  const theme = useTheme$1();
  const resolved = children(() => props.children);
  onMount(() => setMounted(true));
  function InternalDrawer() {
    return createComponent(
      DrawerPaper,
      mergeProps(
        {
          get elevation() {
            return props.variant === "temporary" ? props.elevation : 0;
          },
          square: true,
        },
        () => props.PaperProps,
        {
          get ["class"]() {
            return clsx(classes.paper, props.PaperProps.class);
          },
          ownerState: allProps,
          get children() {
            return resolved();
          },
        },
      ),
    );
  }
  function SlidingDrawer() {
    const anchorInvariant = getAnchor(theme, props.anchor);
    return createComponent(
      Slide,
      mergeProps(
        {
          get ["in"]() {
            return props.open;
          },
          get direction() {
            return oppositeDirection[anchorInvariant];
          },
          get timeout() {
            return props.transitionDuration;
          },
          get appear() {
            return mounted();
          },
        },
        () => props.SlideProps ?? {},
        {
          get children() {
            return createComponent(InternalDrawer, {});
          },
        },
      ),
    );
  }
  return createComponent(Switch$1, {
    get children() {
      return [
        createComponent(Match, {
          get when() {
            return props.variant === "permanent";
          },
          get children() {
            return createComponent(
              DrawerDockedRoot,
              mergeProps(otherProps, {
                get ["class"]() {
                  return clsx(classes.root, classes.docked, otherProps.class);
                },
                ownerState: allProps,
                ref: element,
                get children() {
                  return createComponent(InternalDrawer, {});
                },
              }),
            );
          },
        }),
        createComponent(Match, {
          get when() {
            return props.variant === "persistent";
          },
          get children() {
            return createComponent(
              DrawerDockedRoot,
              mergeProps(otherProps, {
                get ["class"]() {
                  return clsx(classes.root, classes.docked, otherProps.class);
                },
                ownerState: allProps,
                ref: element,
                get children() {
                  return createComponent(SlidingDrawer, {});
                },
              }),
            );
          },
        }),
        createMemo(() =>
          createComponent(Match, {
            get when() {
              return props.variant === "temporary";
            },
            get children() {
              return createComponent(
                DrawerRoot,
                mergeProps(
                  {
                    get BackdropProps() {
                      return {
                        ...(otherProps.BackdropProps ?? {}),
                        ...(props.ModalProps.BackdropProps ?? {}),
                        transitionDuration: props.transitionDuration,
                      };
                    },
                    get ["class"]() {
                      return clsx(
                        classes.root,
                        classes.modal,
                        otherProps.class,
                      );
                    },
                    get open() {
                      return props.open;
                    },
                    ownerState: allProps,
                    get onClose() {
                      return props.onClose;
                    },
                    get hideBackdrop() {
                      return otherProps.hideBackdrop;
                    },
                    ref: element,
                  },
                  otherProps ?? {},
                  () => props.ModalProps ?? {},
                  {
                    transition: true,
                    get children() {
                      return createComponent(SlidingDrawer, {});
                    },
                  },
                ),
              );
            },
          }),
        ),
      ];
    },
  });
});

function getFabUtilityClass(slot) {
  return generateUtilityClass("MuiFab", slot);
}
const fabClasses = generateUtilityClasses("MuiFab", [
  "root",
  "primary",
  "secondary",
  "extended",
  "circular",
  "focusVisible",
  "disabled",
  "colorInherit",
  "sizeSmall",
  "sizeMedium",
  "sizeLarge",
  "info",
  "error",
  "warning",
  "success",
]);

const $$P = createComponentFactory()({
  name: "MuiFab",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disabled",
    "disableFocusRipple",
    "disableRipple",
    "href",
    "size",
    "variant",
  ],
  utilityClass: getFabUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      `size${capitalize(ownerState.size)}`,
      ownerState.color === "inherit" ? "colorInherit" : ownerState.color,
    ],
  }),
});
const FabRoot = styled$1(ButtonBase, {
  name: "MuiFab",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[ownerState.variant],
      styles[`size${capitalize(ownerState.size)}`],
      ownerState.color === "inherit" && styles.colorInherit,
      styles[capitalize(ownerState.size)],
      styles[ownerState.color],
    ];
  },
})(
  ({ theme, ownerState }) => ({
    ...theme.typography.button,
    minHeight: 36,
    transition: theme.transitions.create(
      ["background-color", "box-shadow", "border-color"],
      {
        duration: theme.transitions.duration.short,
      },
    ),
    borderRadius: "50%",
    padding: 0,
    minWidth: 0,
    width: 56,
    height: 56,
    boxShadow: theme.shadows[6],
    "&:active": {
      boxShadow: theme.shadows[12],
    },
    color: theme.palette.getContrastText(theme.palette.grey[300]),
    backgroundColor: theme.palette.grey[300],
    "&:hover": {
      backgroundColor: theme.palette.grey.A100,
      // Reset on touch devices, it doesn't add specificity
      "@media (hover: none)": {
        backgroundColor: theme.palette.grey[300],
      },
      textDecoration: "none",
    },
    [`&.${fabClasses.focusVisible}`]: {
      boxShadow: theme.shadows[6],
    },
    [`&.${fabClasses.disabled}`]: {
      color: theme.palette.action.disabled,
      boxShadow: theme.shadows[0],
      backgroundColor: theme.palette.action.disabledBackground,
    },
    ...(ownerState.size === "small" && {
      width: 40,
      height: 40,
    }),
    ...(ownerState.size === "medium" && {
      width: 48,
      height: 48,
    }),
    ...(ownerState.variant === "extended" && {
      borderRadius: 48 / 2,
      padding: "0 16px",
      width: "auto",
      minHeight: "auto",
      minWidth: 48,
      height: 48,
    }),
    ...(ownerState.variant === "extended" &&
      ownerState.size === "small" && {
        width: "auto",
        padding: "0 8px",
        borderRadius: 34 / 2,
        minWidth: 34,
        height: 34,
      }),
    ...(ownerState.variant === "extended" &&
      ownerState.size === "medium" && {
        width: "auto",
        padding: "0 16px",
        borderRadius: 40 / 2,
        minWidth: 40,
        height: 40,
      }),
    ...(ownerState.color === "inherit" && {
      color: "inherit",
    }),
  }),
  ({ theme, ownerState }) => ({
    ...(ownerState.color !== "inherit" &&
      ownerState.color !== "default" &&
      theme.palette[ownerState.color] != null && {
        color: theme.palette[ownerState.color].contrastText,
        backgroundColor: theme.palette[ownerState.color].main,
        "&:hover": {
          backgroundColor: theme.palette[ownerState.color].dark,
          // Reset on touch devices, it doesn't add specificity
          "@media (hover: none)": {
            backgroundColor: theme.palette[ownerState.color].main,
          },
        },
      }),
  }),
);

/**
 *
 * Demos:
 *
 * - [Floating Action Button](https://mui.com/components/floating-action-button/)
 *
 * API:
 *
 * - [Fab API](https://mui.com/api/fab/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */
const Fab = $$P.defineComponent(function Fab(inProps) {
  const props = $$P.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "children",
    "class",
    "color",
    "component",
    "disabled",
    "disableFocusRipple",
    "focusVisibleClassName",
    "size",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      color: "default",
      component: "button",
      disabled: false,
      disableFocusRipple: false,
      size: "large",
      variant: "circular",
    },
    props,
  );
  const ownerState = mergeProps(props, {
    get color() {
      return baseProps.color;
    },
    get component() {
      return baseProps.component;
    },
    get disabled() {
      return baseProps.disabled;
    },
    get disableFocusRipple() {
      return baseProps.disableFocusRipple;
    },
    get size() {
      return baseProps.size;
    },
    get variant() {
      return baseProps.variant;
    },
  });
  const classes = $$P.useClasses(ownerState);
  return createComponent(
    FabRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
        get component() {
          return baseProps.component;
        },
        get disabled() {
          return baseProps.disabled;
        },
        get focusRipple() {
          return !baseProps.disableFocusRipple;
        },
        get focusVisibleClassName() {
          return clsx(props.focusVisibleClassName);
        },
        ownerState: ownerState,
      },
      other,
      {
        get children() {
          return props.children;
        },
      },
    ),
  );
});

function formControlState(data) {
  const compose = () => {
    return data.states.reduce((acc, state) => {
      acc[state] = data.props[state];
      if (data.muiFormControl) {
        if (typeof data.props[state] === "undefined") {
          acc[state] = data.muiFormControl[state];
        }
      }
      return acc;
    }, {});
  };
  const object = createMutable({});
  createComputed(() => {
    const newObject = compose();
    batch(() => {
      for (const key in newObject) {
        if (object[key] !== newObject[key]) object[key] = newObject[key];
      }
      const newKeys = Object.keys(newObject);
      for (const key in object) {
        if (!newKeys.includes(key)) delete object[key];
      }
    });
  });
  return object;
}

function getInputBaseUtilityClass(slot) {
  return generateUtilityClass("MuiInputBase", slot);
}
const inputBaseClasses = generateUtilityClasses("MuiInputBase", [
  "root",
  "formControl",
  "focused",
  "disabled",
  "adornedStart",
  "adornedEnd",
  "error",
  "sizeSmall",
  "multiline",
  "colorSecondary",
  "fullWidth",
  "hiddenLabel",
  "input",
  "inputSizeSmall",
  "inputMultiline",
  "inputTypeSearch",
  "inputAdornedStart",
  "inputAdornedEnd",
  "inputHiddenLabel",
]);

function hasValue(value) {
  return value != null && !(Array.isArray(value) && value.length === 0);
}
function isFilled(obj, SSR = false) {
  return (
    obj &&
    ((hasValue(obj.value) && obj.value !== "") ||
      (SSR && hasValue(obj.defaultValue) && obj.defaultValue !== ""))
  );
}

function createEffectWithCleaning(effect) {
  let lastCleanup;
  onCleanup(() => lastCleanup?.());
  createEffect((prevCleanup) => {
    if (prevCleanup) {
      lastCleanup = undefined;
      prevCleanup?.();
    }
    return (lastCleanup = effect());
  });
}

const _tmpl$$8 = /*#__PURE__*/ template(`<textarea>`),
  _tmpl$2$1 = /*#__PURE__*/ template(
    `<textarea aria-hidden readonly tabindex="-1">`,
  );
const $$O = createComponentFactory()({
  name: "MuiTextareaAutosize",
  selfPropNames: ["ref", "maxRows", "minRows"],
});
function getStyleValue(computedStyle, property) {
  return parseInt(computedStyle[property], 10) || 0;
}
const styles$1 = {
  shadow: {
    // Visibility needed to hide the extra text area on iPads
    visibility: "hidden",
    // Remove from the content flow
    position: "absolute",
    // Ignore the scrollbar width
    overflow: "hidden",
    height: 0,
    top: 0,
    left: 0,
    // Create a new layer, increase the isolation of the computed values
    transform: "translateZ(0)",
  },
};
const TextareaAutosize = $$O.defineComponent(function (props) {
  const ref = createRef(props);
  const [, other] = splitProps(props, ["maxRows", "minRows", "style", "value"]);
  const baseProps = mergeProps(
    {
      minRows: 1,
    },
    props,
  );
  let shadowRef;
  const renders = {
    current: 0,
  };
  const [state, setState] = createSignal({});
  const syncHeight = () => {
    const input = ref.current;
    const containerWindow = ownerWindow(input);
    const computedStyle = containerWindow.getComputedStyle(input);
    // If input's width is shrunk and it's not visible, don't sync height.
    if (computedStyle.width === "0px") {
      return;
    }
    shadowRef.style.width = computedStyle.width;
    shadowRef.value = input.value || props.placeholder || "x";
    if (shadowRef.value.slice(-1) === "\n") {
      // Certain fonts which overflow the line height will cause the textarea
      // to report a different scrollHeight depending on whether the last line
      // is empty. Make it non-empty to avoid this issue.
      shadowRef.value += " ";
    }
    const boxSizing = computedStyle["boxSizing"];
    const padding =
      getStyleValue(computedStyle, "paddingBottom") +
      getStyleValue(computedStyle, "paddingTop");
    const border =
      getStyleValue(computedStyle, "borderBottomWidth") +
      getStyleValue(computedStyle, "borderTopWidth");
    // The height of the inner content
    const innerHeight = shadowRef.scrollHeight;
    // Measure height of a textarea with a single row
    shadowRef.value = "x";
    const singleRowHeight = shadowRef.scrollHeight;
    // The height of the outer content
    let outerHeight = innerHeight;
    if (baseProps.minRows) {
      outerHeight = Math.max(
        Number(baseProps.minRows) * singleRowHeight,
        outerHeight,
      );
    }
    if (props.maxRows) {
      outerHeight = Math.min(
        Number(props.maxRows) * singleRowHeight,
        outerHeight,
      );
    }
    outerHeight = Math.max(outerHeight, singleRowHeight);
    // Take the box sizing into account for applying this value as a style.
    const outerHeightStyle =
      outerHeight + (boxSizing === "border-box" ? padding + border : 0);
    const overflow = Math.abs(outerHeight - innerHeight) <= 1;
    setState((prevState) => {
      // Need a large enough difference to update the height.
      // This prevents infinite rendering loop.
      if (
        renders.current < 20 &&
        ((outerHeightStyle > 0 &&
          Math.abs((prevState.outerHeightStyle || 0) - outerHeightStyle) > 1) ||
          prevState.overflow !== overflow)
      ) {
        renders.current += 1;
        return {
          overflow,
          outerHeightStyle,
        };
      }
      return prevState;
    });
  };
  createEffectWithCleaning(() => {
    syncHeight();
    const handleResize = debounce$1(() => {
      renders.current = 0;
      syncHeight();
    });
    const containerWindow = ownerWindow(ref.current);
    containerWindow.addEventListener("resize", handleResize);
    let resizeObserver;
    if (typeof ResizeObserver !== "undefined") {
      resizeObserver = new ResizeObserver(handleResize);
      resizeObserver.observe(ref.current);
    }
    return () => {
      handleResize.clear();
      containerWindow.removeEventListener("resize", handleResize);
      if (resizeObserver) {
        resizeObserver.disconnect();
      }
    };
  });
  createEffect(
    on(
      () => [props.value],
      () => {
        renders.current = 0;
        syncHeight();
      },
    ),
  );
  const style1 = mergeProps(
    {
      get height() {
        return `${state().outerHeightStyle}px`;
      },
      get overflow() {
        return state().overflow ? "hidden" : null;
      },
    },
    () => props.style,
  );
  const style2 = mergeProps(styles$1.shadow, () => props.style, {
    padding: 0,
  });
  return [
    (() => {
      const _el$ = _tmpl$$8();
      use(ref, _el$);
      spread(
        _el$,
        mergeProps(
          {
            get rows() {
              return baseProps.minRows;
            },
          },
          other,
        ),
        false,
        false,
      );
      createRenderEffect((_$p) => style(_el$, style1, _$p));
      return _el$;
    })(),
    (() => {
      const _el$2 = _tmpl$2$1();
      const _ref$ = shadowRef;
      typeof _ref$ === "function" ? use(_ref$, _el$2) : (shadowRef = _el$2);
      createRenderEffect(
        (_p$) => {
          const _v$ = props.class,
            _v$2 = style2;
          _v$ !== _p$._v$ && className(_el$2, (_p$._v$ = _v$));
          _p$._v$2 = style(_el$2, _v$2, _p$._v$2);
          return _p$;
        },
        {
          _v$: undefined,
          _v$2: undefined,
        },
      );
      return _el$2;
    })(),
  ];
}, false);

const $$N = createComponentFactory()({
  name: "MuiInputBase",
  propDefaults: ({ set }) =>
    set({
      components: {},
      componentsProps: {},
      fullWidth: false,
      inputComponent: "input",
      inputProps: {},
      multiline: false,
      type: "text",
      disableInjectingGlobalStyles: false,
    }),
  selfPropNames: [
    "aria-describedby",
    "autoComplete",
    "autoFocus",
    "classes",
    "color",
    "components",
    "componentsProps",
    "defaultValue",
    "disableInjectingGlobalStyles",
    "disabled",
    "endAdornment",
    "error",
    "fullWidth",
    "id",
    "inputComponent",
    "inputProps",
    "inputProps",
    "inputRef",
    "margin",
    "maxRows",
    "minRows",
    "multiline",
    "name",
    "onBlur",
    "onChange",
    "onFocus",
    "onKeyDown",
    "onKeyUp",
    "placeholder",
    "readOnly",
    "renderSuffix",
    "required",
    "rows",
    "size",
    "startAdornment",
    "type",
    "value",
  ],
  utilityClass: getInputBaseUtilityClass,
  autoCallUseClasses: false,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      `color${capitalize(ownerState.color)}`,
      !!ownerState.disabled && "disabled",
      !!ownerState.error && "error",
      !!ownerState.fullWidth && "fullWidth",
      ownerState.focused && "focused",
      !!ownerState.formControl && "formControl",
      ownerState.size === "small" && "sizeSmall",
      ownerState.multiline && "multiline",
      !!ownerState.startAdornment && "adornedStart",
      !!ownerState.endAdornment && "adornedEnd",
      !!ownerState.hiddenLabel && "hiddenLabel",
    ],
    input: [
      "input",
      !!ownerState.disabled && "disabled",
      ownerState.type === "search" && "inputTypeSearch",
      ownerState.multiline && "inputMultiline",
      ownerState.size === "small" && "inputSizeSmall",
      !!ownerState.hiddenLabel && "inputHiddenLabel",
      !!ownerState.startAdornment && "inputAdornedStart",
      !!ownerState.endAdornment && "inputAdornedEnd",
    ],
  }),
});
const rootOverridesResolver = (props, styles) => {
  const ownerState = props.ownerState;
  return [
    styles.root,
    !!ownerState.formControl && styles.formControl,
    !!ownerState.startAdornment && styles.adornedStart,
    !!ownerState.endAdornment && styles.adornedEnd,
    !!ownerState.error && styles.error,
    ownerState.size === "small" && styles.sizeSmall,
    ownerState.multiline && styles.multiline,
    ownerState.color && styles[`color${capitalize(ownerState.color)}`],
    !!ownerState.fullWidth && styles.fullWidth,
    !!ownerState.hiddenLabel && styles.hiddenLabel,
  ];
};
const inputOverridesResolver = (props, styles) => {
  const ownerState = props.ownerState;
  return [
    styles.input,
    ownerState.size === "small" && styles.inputSizeSmall,
    ownerState.multiline && styles.inputMultiline,
    ownerState.type === "search" && styles.inputTypeSearch,
    !!ownerState.startAdornment && styles.inputAdornedStart,
    !!ownerState.endAdornment && styles.inputAdornedEnd,
    !!ownerState.hiddenLabel && styles.inputHiddenLabel,
  ];
};
const InputBaseRoot = styled$1("div", {
  name: "MuiInputBase",
  slot: "Root",
  overridesResolver: rootOverridesResolver,
})(({ theme, ownerState }) => ({
  ...theme.typography.body1,
  color: theme.palette.text.primary,
  lineHeight: "1.4375em",
  // 23px
  boxSizing: "border-box",
  // Prevent padding issue with fullWidth.
  position: "relative",
  cursor: "text",
  display: "inline-flex",
  alignItems: "center",
  [`&.${inputBaseClasses.disabled}`]: {
    color: theme.palette.text.disabled,
    cursor: "default",
  },
  ...(ownerState.multiline && {
    padding: "4px 0 5px",
    ...(ownerState.size === "small" && {
      paddingTop: 1,
    }),
  }),
  ...(ownerState.fullWidth && {
    width: "100%",
  }),
}));
const InputBaseComponent = styled$1("input", {
  name: "MuiInputBase",
  slot: "Input",
  overridesResolver: inputOverridesResolver,
})(({ theme, ownerState }) => {
  const light = theme.palette.mode === "light";
  const placeholder = {
    color: "currentColor",
    opacity: light ? 0.42 : 0.5,
    transition: theme.transitions.create("opacity", {
      duration: theme.transitions.duration.shorter,
    }),
  };
  const placeholderHidden = {
    opacity: "0 !important",
  };
  const placeholderVisible = {
    opacity: light ? 0.42 : 0.5,
  };
  return {
    font: "inherit",
    letterSpacing: "inherit",
    color: "currentColor",
    padding: "4px 0 5px",
    border: 0,
    boxSizing: "content-box",
    background: "none",
    height: "1.4375em",
    // Reset 23pxthe native input line-height
    margin: 0,
    // Reset for Safari
    WebkitTapHighlightColor: "transparent",
    display: "block",
    // Make the flex item shrink with Firefox
    minWidth: 0,
    width: "100%",
    // Fix IE11 width issue
    animationName: "mui-auto-fill-cancel",
    animationDuration: "10ms",
    "&::-webkit-input-placeholder": placeholder,
    "&::-moz-placeholder": placeholder,
    // Firefox 19+
    "&:-ms-input-placeholder": placeholder,
    // IE11
    "&::-ms-input-placeholder": placeholder,
    // Edge
    "&:focus": {
      outline: 0,
    },
    // Reset Firefox invalid required input style
    "&:invalid": {
      boxShadow: "none",
    },
    "&::-webkit-search-decoration": {
      // Remove the padding when type=search.
      WebkitAppearance: "none",
    },
    // Show and hide the placeholder logic
    [`label[data-shrink=false] + .${inputBaseClasses.formControl} &`]: {
      "&::-webkit-input-placeholder": placeholderHidden,
      "&::-moz-placeholder": placeholderHidden,
      // Firefox 19+
      "&:-ms-input-placeholder": placeholderHidden,
      // IE11
      "&::-ms-input-placeholder": placeholderHidden,
      // Edge
      "&:focus::-webkit-input-placeholder": placeholderVisible,
      "&:focus::-moz-placeholder": placeholderVisible,
      // Firefox 19+
      "&:focus:-ms-input-placeholder": placeholderVisible,
      // IE11
      "&:focus::-ms-input-placeholder": placeholderVisible, // Edge
    },

    [`&.${inputBaseClasses.disabled}`]: {
      opacity: 1,
      // Reset iOS opacity
      WebkitTextFillColor: theme.palette.text.disabled, // Fix opacity Safari bug
    },

    "&:-webkit-autofill": {
      animationDuration: "5000s",
      animationName: "mui-auto-fill",
    },
    ...(ownerState.size === "small" && {
      paddingTop: 1,
    }),
    ...(ownerState.multiline && {
      height: "auto",
      resize: "none",
      padding: 0,
      paddingTop: 0,
    }),
    ...(ownerState.type === "search" && {
      // Improve type search style.
      MozAppearance: "textfield",
    }),
  };
});
const inputGlobalStyles = () =>
  createComponent(GlobalStyles, {
    styles: {
      "@keyframes mui-auto-fill": {
        from: {
          display: "block",
        },
      },
      "@keyframes mui-auto-fill-cancel": {
        from: {
          display: "block",
        },
      },
    },
  });
const selectionTypes = new Set(["text", "search", "password", "tel", "url"]);

/**
 * `InputBase` contains as few styles as possible.
 * It aims to be a simple building block for creating an input.
 * It contains a load of style reset and some state logic.
 *
 * Demos:
 *
 * - [Text Fields](https://mui.com/components/text-fields/)
 *
 * API:
 *
 * - [InputBase API](https://mui.com/api/input-base/)
 */
const InputBase = $$N.component(function InputBase({
  allProps,
  otherProps,
  props,
}) {
  const inputValue = () =>
    props.inputProps.value != null ? props.inputProps.value : props.value;
  const isControlled = (inputValue() ?? null) !== null;
  const attrValueOnBlur = () =>
    inputRef.ref.type === "date" || inputRef.ref.type === "number";
  const hasSelectionRange = () =>
    inputRef.ref.nodeName === "TEXTAREA" ||
    !inputRef.ref.type ||
    selectionTypes.has(inputRef.ref.type);
  const [value, setValue] = useControlled({
    controlled: () => inputValue(),
    default: () => props.defaultValue,
    name: "InputBase",
  });
  const initialValue = value();
  const attrValue = () =>
    typeof InputComponent() === "string" ? initialValue : value();
  const inputRef = createRef({
    ref: (instance) => {
      if (typeof props.inputRef === "function") props.inputRef(instance);
    },
  });
  let lastSelectionStart;
  let controlledValueUpdated = false;
  onMount(() => {
    const isElement = inputRef.ref instanceof HTMLElement;
    if (isControlled && isElement)
      inputRef.ref.addEventListener("blur", () => {
        if (attrValueOnBlur()) {
          inputRef.ref.setAttribute("value", value() ?? "");
        }
      });
    inputRef.ref.addEventListener("input", (event) => {
      const nodeValue = inputRef.ref.value;
      if (isElement) {
        const start = inputRef.ref.selectionStart ?? nodeValue.length;
        lastSelectionStart = start;
      }
      controlledValueUpdated = false;
      if (typeof props.inputProps.onChange === "function") {
        props.inputProps.onChange(event);
      }
      setValue(nodeValue);
      if (typeof props.onChange === "function") {
        props.onChange(event, nodeValue);
      }
      if (isControlled && !controlledValueUpdated) {
        const newValue = isElement ? value() : value() ?? "";
        if (inputRef.ref.value !== newValue) inputRef.ref.value = newValue;
      }
    });
  });
  createEffect((loadDefaultValue) => {
    const input = inputRef.ref;
    if (isControlled || loadDefaultValue) {
      controlledValueUpdated = true;
      const v = value();
      const isInputObject = !(input instanceof HTMLElement);
      if (isInputObject) {
        if (v !== input.value) input.value = v;
      } else if (typeof v === "string" || typeof v === "number") {
        if (input instanceof HTMLTextAreaElement) {
          input.innerText = v;
        } else if (!attrValueOnBlur()) {
          input.setAttribute("value", v);
        }
        if (v !== input.value) input.value = v;
        if (hasSelectionRange()) {
          const selectionStart = lastSelectionStart ?? v.length;
          if (input.selectionStart !== selectionStart)
            input.setSelectionRange(selectionStart, selectionStart);
        }
      }
    }
    return false;
  }, true);
  const [focused, setFocused] = createSignal(false);
  const muiFormControl = useFormControl();
  const partialFcs = formControlState({
    props: allProps,
    muiFormControl,
    states: [
      "color",
      "disabled",
      "error",
      "hiddenLabel",
      "size",
      "required",
      "filled",
    ],
  });
  const fcs = mergeProps(partialFcs, {
    get focused() {
      return muiFormControl ? muiFormControl.focused : focused();
    },
  });

  // The blur won't fire when the disabled state is set on a focused input.
  // We need to book keep the focused state manually.
  createEffect(() => {
    if (!muiFormControl && props.disabled && focused()) {
      setFocused(false);
      if (typeof props.onBlur === "function") props.onBlur?.(null);
    }
  });
  const onFilled = () => muiFormControl && muiFormControl.onFilled;
  const onEmpty = () => muiFormControl && muiFormControl.onEmpty;
  const checkDirty = (obj) => {
    if (isFilled(obj)) {
      onFilled()?.();
    } else {
      onEmpty()?.();
    }
  };
  createRenderEffect(() => {
    if (isControlled) {
      checkDirty({
        value: value(),
      });
    }
  });

  // Check the input state on mount, in case it was filled by the user
  // or auto filled by the browser before the hydration (for SSR).
  onMount(() => {
    checkDirty(inputRef.ref);
  });
  const isMultilineInput = () =>
    props.multiline && props.inputComponent === "input";
  const InputComponent = () =>
    isMultilineInput() ? TextareaAutosize : props.inputComponent;
  const inputProps = createMemo(() => {
    let inputProps = props.inputProps;
    if (isMultilineInput()) {
      if (props.rows) {
        inputProps = {
          type: undefined,
          ["minRows"]: props.rows,
          ["maxRows"]: props.rows,
          ...inputProps,
        };
      } else {
        inputProps = {
          type: undefined,
          ["maxRows"]: props.maxRows,
          ["minRows"]: props.minRows,
          ...inputProps,
        };
      }
    }
    return mergeProps(inputProps, () => props.componentsProps.input || {});
  });
  createEffect(() => {
    muiFormControl?.setAdornedStart(Boolean(props.startAdornment));
  });
  const ownerState = mergeProps(allProps, {
    get color() {
      return fcs.color || "primary";
    },
    get disabled() {
      return fcs.disabled;
    },
    get error() {
      return fcs.error;
    },
    get focused() {
      return fcs.focused;
    },
    get formControl() {
      return muiFormControl;
    },
    get hiddenLabel() {
      return fcs.hiddenLabel;
    },
    get size() {
      return fcs.size;
    },
  });
  const classes = $$N.useClasses(ownerState);
  const Root = () => props.components.Root || InputBaseRoot;
  const rootProps = () => props.componentsProps.root || {};
  const Input = () => props.components.Input || InputBaseComponent;
  const rootOwnerState = mergeProps(
    ownerState,
    () => rootProps()["ownerState"] || {},
  );
  const inputOwnerState = mergeProps(
    ownerState,
    () => inputProps()["ownerState"] || {},
  );
  const renderSuffixProps = mergeProps(fcs, {
    get startAdornment() {
      return props.startAdornment;
    },
  });
  const suffix = createMemo(() => props.renderSuffix?.(renderSuffixProps));
  return [
    createMemo(
      (() => {
        const _c$ = createMemo(() => !!!props.disableInjectingGlobalStyles);
        return () => _c$() && inputGlobalStyles();
      })(),
    ),
    createComponent(
      Dynamic,
      mergeProps(
        rootProps,
        otherProps,
        {
          get $component() {
            return Root();
          },
        },
        () =>
          !isHostComponent(Root()) && {
            ownerState: rootOwnerState,
          },
        {
          onClick: (event) => {
            if (inputRef.ref && event.currentTarget === event.target) {
              inputRef.ref.focus();
            }
            if (typeof otherProps.onClick === "function") {
              otherProps.onClick(event);
            }
          },
          get ["class"]() {
            return clsx(classes.root, rootProps().class, otherProps.class);
          },
          get children() {
            return [
              createMemo(() => props.startAdornment),
              createComponent(FormControlContext.Provider, {
                value: undefined,
                get children() {
                  return createComponent(
                    Dynamic,
                    mergeProps(
                      {
                        get $component() {
                          return Input();
                        },
                        ownerState: ownerState,
                        get ["aria-invalid"]() {
                          return fcs.error;
                        },
                        get ["aria-describedby"]() {
                          return props["aria-describedby"];
                        },
                        get autocomplete() {
                          return props.autoComplete;
                        },
                        get autofocus() {
                          return props.autoFocus;
                        },
                        get disabled() {
                          return fcs.disabled;
                        },
                        get id() {
                          return props.id;
                        },
                        onAnimationStart: (event) => {
                          // Provide a fake value as Chrome might not let you access it for security reasons.
                          checkDirty(
                            event.animationName === "mui-auto-fill-cancel"
                              ? inputRef.ref
                              : {
                                  value: "x",
                                },
                          );
                        },
                        get name() {
                          return props.name;
                        },
                        get placeholder() {
                          return props.placeholder;
                        },
                        get readOnly() {
                          return props.readOnly;
                        },
                        get required() {
                          return fcs.required;
                        },
                        get value() {
                          return attrValue();
                        },
                      },
                      () => ({
                        rows: props.rows,
                      }),
                      {
                        get onKeyDown() {
                          return props.onKeyDown;
                        },
                        get onKeyUp() {
                          return props.onKeyUp;
                        },
                        get type() {
                          return props.type;
                        },
                      },
                      inputProps,
                      () =>
                        !isHostComponent(Input()) && {
                          as: InputComponent(),
                          ownerState: inputOwnerState,
                        },
                      {
                        ref: inputRef,
                        get ["class"]() {
                          return clsx(classes.input, inputProps().class);
                        },
                        onBlur: (event) => {
                          props.onBlur?.(event);
                          if (typeof props.inputProps.onBlur === "function") {
                            props.inputProps.onBlur(event);
                          }
                          if (muiFormControl && muiFormControl.onBlur) {
                            muiFormControl.onBlur();
                          } else {
                            setFocused(false);
                          }
                        },
                        onInput: (event) => {
                          if (!isControlled) {
                            const element = event.target || inputRef.ref;
                            if (element == null) {
                              throw new Error(
                                "MUI: Expected valid input target. " +
                                  "Did you use a custom `inputComponent` and forget to forward refs? " +
                                  "See https://mui.com/r/input-component-ref-interface for more info.",
                              );
                            }
                            checkDirty({
                              value: element.value,
                            });
                          }
                        },
                        onFocus: (event) => {
                          // Fix a bug with IE11 where the focus/blur events are triggered
                          // while the component is disabled.
                          if (fcs.disabled) {
                            event.stopPropagation();
                            return;
                          }
                          if (typeof props.onFocus === "function") {
                            props.onFocus(event);
                          }
                          if (typeof props.inputProps.onFocus === "function") {
                            props.inputProps.onFocus(event);
                          }
                          if (muiFormControl && muiFormControl.onFocus) {
                            muiFormControl.onFocus();
                          } else {
                            setFocused(true);
                          }
                        },
                      },
                    ),
                  );
                },
              }),
              createMemo(() => props.endAdornment),
              createMemo(() => suffix()),
            ];
          },
        },
      ),
    ),
  ];
});

function getFilledInputUtilityClass(slot) {
  return generateUtilityClass("MuiFilledInput", slot);
}
const filledInputClasses = {
  ...inputBaseClasses,
  ...generateUtilityClasses("MuiFilledInput", ["root", "underline", "input"]),
};

const $$M = createComponentFactory()({
  name: "MuiFilledInput",
  propDefaults: ({ set }) =>
    set({
      components: {},
      fullWidth: false,
      inputComponent: "input",
      multiline: false,
      type: "text",
      hiddenLabel: false,
    }),
  selfPropNames: ["classes", "disableUnderline", "hiddenLabel"],
  utilityClass: getFilledInputUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", !ownerState.disableUnderline && "underline"],
    input: ["input"],
  }),
});
const FilledInputRoot = styled$1(InputBaseRoot, {
  /*shouldForwardProp: (prop) =>
    rootShouldForwardProp(prop) || prop === "classes",*/
  name: "MuiFilledInput",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      ...rootOverridesResolver(props, styles),
      !ownerState.disableUnderline && styles.underline,
    ];
  },
})(({ theme, ownerState }) => {
  const light = theme.palette.mode === "light";
  const bottomLineColor = light
    ? "rgba(0, 0, 0, 0.42)"
    : "rgba(255, 255, 255, 0.7)";
  const backgroundColor = light
    ? "rgba(0, 0, 0, 0.06)"
    : "rgba(255, 255, 255, 0.09)";
  return {
    position: "relative",
    backgroundColor,
    borderTopLeftRadius: theme.shape.borderRadius,
    borderTopRightRadius: theme.shape.borderRadius,
    transition: theme.transitions.create("background-color", {
      duration: theme.transitions.duration.shorter,
      easing: theme.transitions.easing.easeOut,
    }),
    "&:hover": {
      backgroundColor: light
        ? "rgba(0, 0, 0, 0.09)"
        : "rgba(255, 255, 255, 0.13)",
      // Reset on touch devices, it doesn't add specificity
      "@media (hover: none)": {
        backgroundColor,
      },
    },
    [`&.${filledInputClasses.focused}`]: {
      backgroundColor,
    },
    [`&.${filledInputClasses.disabled}`]: {
      backgroundColor: light
        ? "rgba(0, 0, 0, 0.12)"
        : "rgba(255, 255, 255, 0.12)",
    },
    ...(!ownerState.disableUnderline && {
      "&:after": {
        borderBottom: `2px solid ${theme.palette[ownerState.color].main}`,
        left: 0,
        bottom: 0,
        // Doing the other way around crash on IE11 "''" https://github.com/cssinjs/jss/issues/242
        content: '""',
        position: "absolute",
        right: 0,
        transform: "scaleX(0)",
        transition: theme.transitions.create("transform", {
          duration: theme.transitions.duration.shorter,
          easing: theme.transitions.easing.easeOut,
        }),
        pointerEvents: "none", // Transparent to the hover style.
      },

      [`&.${filledInputClasses.focused}:after`]: {
        transform: "scaleX(1)",
      },
      [`&.${filledInputClasses.error}:after`]: {
        borderBottomColor: theme.palette.error.main,
        transform: "scaleX(1)", // error is always underlined in red
      },

      "&:before": {
        borderBottom: `1px solid ${bottomLineColor}`,
        left: 0,
        bottom: 0,
        // Doing the other way around crash on IE11 "''" https://github.com/cssinjs/jss/issues/242
        content: '"\\00a0"',
        position: "absolute",
        right: 0,
        transition: theme.transitions.create("border-bottom-color", {
          duration: theme.transitions.duration.shorter,
        }),
        pointerEvents: "none", // Transparent to the hover style.
      },

      [`&:hover:not(.${filledInputClasses.disabled}):before`]: {
        borderBottom: `1px solid ${theme.palette.text.primary}`,
      },
      [`&.${filledInputClasses.disabled}:before`]: {
        borderBottomStyle: "dotted",
      },
    }),
    ...(ownerState.startAdornment && {
      paddingLeft: 12,
    }),
    ...(ownerState.endAdornment && {
      paddingRight: 12,
    }),
    ...(ownerState.multiline && {
      padding: "25px 12px 8px",
      ...(ownerState.size === "small" && {
        paddingTop: 21,
        paddingBottom: 4,
      }),
      ...(ownerState.hiddenLabel && {
        paddingTop: 16,
        paddingBottom: 17,
      }),
    }),
  };
});
const FilledInputInput = styled$1(InputBaseComponent, {
  name: "MuiFilledInput",
  slot: "Input",
  overridesResolver: inputOverridesResolver,
})(({ theme, ownerState }) => ({
  paddingTop: 25,
  paddingRight: 12,
  paddingBottom: 8,
  paddingLeft: 12,
  "&:-webkit-autofill": {
    WebkitBoxShadow:
      theme.palette.mode === "light" ? null : "0 0 0 100px #266798 inset",
    WebkitTextFillColor: theme.palette.mode === "light" ? null : "#fff",
    caretColor: theme.palette.mode === "light" ? null : "#fff",
    borderTopLeftRadius: "inherit",
    borderTopRightRadius: "inherit",
  },
  ...(ownerState.size === "small" && {
    paddingTop: 21,
    paddingBottom: 4,
  }),
  ...(ownerState.hiddenLabel && {
    paddingTop: 16,
    paddingBottom: 17,
  }),
  ...(ownerState.multiline && {
    paddingTop: 0,
    paddingBottom: 0,
    paddingLeft: 0,
    paddingRight: 0,
  }),
  ...(ownerState.startAdornment && {
    paddingLeft: 0,
  }),
  ...(ownerState.endAdornment && {
    paddingRight: 0,
  }),
  ...(ownerState.hiddenLabel &&
    ownerState.size === "small" && {
      paddingTop: 8,
      paddingBottom: 9,
    }),
}));
const FilledInput = $$M.component(function FilledInput({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const componentProps = createMemo(() => {
    const filledInputComponentsProps = {
      root: {
        ownerState: allProps,
      },
      input: {
        ownerState: allProps,
      },
    };
    return otherProps.componentsProps
      ? deepmerge(otherProps.componentsProps, filledInputComponentsProps)
      : filledInputComponentsProps;
  });
  const allClasses = mergeProps(() => props.classes || {}, classes);
  return createComponent(
    InputBase,
    mergeProps(otherProps, {
      get components() {
        return {
          Root: FilledInputRoot,
          Input: FilledInputInput,
          ...(otherProps.components || {}),
        };
      },
      get componentsProps() {
        return componentProps();
      },
      classes: allClasses,
    }),
  );
});

function getFormControlUtilityClasses(slot) {
  return generateUtilityClass("MuiFormControl", slot);
}
generateUtilityClasses("MuiFormControl", [
  "root",
  "marginNone",
  "marginNormal",
  "marginDense",
  "fullWidth",
  "disabled",
]);

const $$L = createComponentFactory()({
  name: "MuiFormControl",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disabled",
    "error",
    "focused",
    "fullWidth",
    "hiddenLabel",
    "margin",
    "required",
    "size",
    "variant",
  ],
  propDefaults: ({ set }) =>
    set({
      color: "primary",
      component: "div",
      disabled: false,
      error: false,
      fullWidth: false,
      hiddenLabel: false,
      margin: "none",
      required: false,
      size: "medium",
      variant: "outlined",
    }),
  utilityClass: getFormControlUtilityClasses,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.margin !== "none" && `margin${capitalize(ownerState.margin)}`,
      ownerState.fullWidth && "fullWidth",
    ],
  }),
});
const FormControlRoot = styled$1("div", {
  name: "MuiFormControl",
  slot: "Root",
  overridesResolver: ({ ownerState }, styles) => {
    return {
      ...styles.root,
      ...styles[`margin${capitalize(ownerState.margin)}`],
      ...(ownerState.fullWidth && styles.fullWidth),
    };
  },
})(({ ownerState }) => ({
  display: "inline-flex",
  flexDirection: "column",
  position: "relative",
  // Reset fieldset default style.
  minWidth: 0,
  padding: 0,
  margin: 0,
  border: 0,
  verticalAlign: "top",
  // Fix alignment issue on Safari.
  ...(ownerState.margin === "normal" && {
    marginTop: 16,
    marginBottom: 8,
  }),
  ...(ownerState.margin === "dense" && {
    marginTop: 8,
    marginBottom: 4,
  }),
  ...(ownerState.fullWidth && {
    width: "100%",
  }),
}));

/**
 * Provides context such as filled/focused/error/required for form inputs.
 * Relying on the context provides high flexibility and ensures that the state always stays
 * consistent across the children of the `FormControl`.
 * This context is used by the following components:
 *
 * *   FormLabel
 * *   FormHelperText
 * *   Input
 * *   InputLabel
 *
 * You can find one composition example below and more going to [the demos](https://mui.com/components/text-fields/#components).
 *
 * ```jsx
 * <FormControl>
 *   <InputLabel for="my-input">Email address</InputLabel>
 *   <Input id="my-input" aria-describedby="my-helper-text" />
 *   <FormHelperText id="my-helper-text">We'll never share your email.</FormHelperText>
 * </FormControl>
 * ```
 *
 * ⚠️ Only one `InputBase` can be used within a FormControl because it create visual inconsistencies.
 * For instance, only one input can be focused at the same time, the state shouldn't be shared.
 *
 * Demos:
 *
 * - [Checkboxes](https://mui.com/components/checkboxes/)
 * - [Radio Buttons](https://mui.com/components/radio-buttons/)
 * - [Switches](https://mui.com/components/switches/)
 * - [Text Fields](https://mui.com/components/text-fields/)
 *
 * API:
 *
 * - [FormControl API](https://mui.com/api/form-control/)
 */
const FormControl = $$L.component(function FormControl({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const [adornedStart, setAdornedStart] = createSignal(false);
  const [filled, setFilled] = createSignal(false);
  const [focusedState, setFocused] = createSignal(false);
  createEffect(() => {
    if (props.disabled && focusedState()) setFocused(false);
  });
  const focused = () =>
    props.focused !== undefined && !props.disabled
      ? props.focused
      : focusedState();
  let registerEffect;
  return createComponent(FormControlContext.Provider, {
    value: {
      get adornedStart() {
        return adornedStart();
      },
      setAdornedStart,
      get margin() {
        return props.margin;
      },
      get color() {
        return props.color;
      },
      get disabled() {
        return props.disabled;
      },
      get error() {
        return props.error;
      },
      get filled() {
        return filled();
      },
      get focused() {
        return focused();
      },
      get fullWidth() {
        return props.fullWidth;
      },
      get hiddenLabel() {
        return props.hiddenLabel;
      },
      get size() {
        return props.size;
      },
      onBlur: () => {
        setFocused(false);
      },
      onEmpty: () => {
        setFilled(false);
      },
      onFilled: () => {
        setFilled(true);
      },
      onFocus: () => {
        setFocused(true);
      },
      registerEffect,
      get required() {
        return props.required;
      },
      get variant() {
        return props.variant;
      },
    },
    get children() {
      return createComponent(
        FormControlRoot,
        mergeProps(otherProps, {
          ownerState: allProps,
          get ["class"]() {
            return clsx(classes.root, otherProps.class);
          },
          get children() {
            return props.children;
          },
        }),
      );
    },
  });
});

function getFormControlLabelUtilityClasses(slot) {
  return generateUtilityClass("MuiFormControlLabel", slot);
}
const formControlLabelClasses = generateUtilityClasses("MuiFormControlLabel", [
  "root",
  "labelPlacementStart",
  "labelPlacementTop",
  "labelPlacementBottom",
  "disabled",
  "label",
  "error",
]);

const $$K = createComponentFactory()({
  name: "MuiFormControlLabel",
  propDefaults: ({ set }) =>
    set({
      componentsProps: {},
      labelPlacement: "end",
    }),
  selfPropNames: [
    "checked",
    "classes",
    "componentsProps",
    "control",
    "disableTypography",
    "disabled",
    "inputRef",
    "label",
    "labelPlacement",
    "name",
    "onChange",
    "value",
  ],
  autoCallUseClasses: false,
  utilityClass: getFormControlLabelUtilityClasses,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !!ownerState.disabled && "disabled",
      `labelPlacement${capitalize(ownerState.labelPlacement)}`,
      !!ownerState.error && "error",
    ],
    label: ["label", !!ownerState.disabled && "disabled"],
  }),
});
const FormControlLabelRoot = styled$1("label", {
  name: "MuiFormControlLabel",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      {
        [`& .${formControlLabelClasses.label}`]: styles.label,
      },
      styles.root,
      styles[`labelPlacement${capitalize(ownerState.labelPlacement)}`],
    ];
  },
})(({ theme, ownerState }) => ({
  display: "inline-flex",
  alignItems: "center",
  cursor: "pointer",
  // For correct alignment with the text.
  verticalAlign: "middle",
  WebkitTapHighlightColor: "transparent",
  marginLeft: -11,
  marginRight: 16,
  // used for row presentation of radio/checkbox
  [`&.${formControlLabelClasses.disabled}`]: {
    cursor: "default",
  },
  ...(ownerState.labelPlacement === "start" && {
    flexDirection: "row-reverse",
    marginLeft: 16,
    // used for row presentation of radio/checkbox
    marginRight: -11,
  }),
  ...(ownerState.labelPlacement === "top" && {
    flexDirection: "column-reverse",
    marginLeft: 16,
  }),
  ...(ownerState.labelPlacement === "bottom" && {
    flexDirection: "column",
    marginLeft: 16,
  }),
  [`& .${formControlLabelClasses.label}`]: {
    [`&.${formControlLabelClasses.disabled}`]: {
      color: theme.palette.text.disabled,
    },
  },
}));

/**
 * Drop-in replacement of the `Radio`, `Switch` and `Checkbox` component.
 * Use this component if you want to display an extra label.
 *
 * Demos:
 *
 * - [Checkboxes](https://mui.com/components/checkboxes/)
 * - [Radio Buttons](https://mui.com/components/radio-buttons/)
 * - [Switches](https://mui.com/components/switches/)
 *
 * API:
 *
 * - [FormControlLabel API](https://mui.com/api/form-control-label/)
 */
const FormControlLabel = $$K.component(function FormControlLabel({
  allProps,
  otherProps,
  props,
}) {
  const muiFormControl = useFormControl();
  const [partialContextProps] = splitProps(props, [
    "checked",
    "name",
    "onChange",
    "value",
    "inputRef",
    "disabled",
  ]);
  const [childDisabled, setChildDisabled] = createSignal(props.disabled);
  const contextProps = mergeProps(partialContextProps, {
    setDisabled: (state) => setChildDisabled(state),
  });
  const fcs = formControlState({
    props: allProps,
    muiFormControl,
    states: ["error"],
  });
  const ownerState = mergeProps(allProps, {
    get error() {
      return fcs.error;
    },
    get disabled() {
      return childDisabled();
    },
  });
  const classes = $$K.useClasses(ownerState);
  const Label = () => {
    const label = children(() => props.label);
    return createComponent(Show, {
      get when() {
        return isSuidElement(label(), Typography) || props.disableTypography;
      },
      get fallback() {
        return createComponent(
          Typography,
          mergeProps(
            {
              component: "span",
              get ["class"]() {
                return classes.label;
              },
            },
            () => props.componentsProps.typography,
            {
              get children() {
                return label();
              },
            },
          ),
        );
      },
      get children() {
        return label();
      },
    });
  };
  return createComponent(FormControlLabelContext.Provider, {
    value: contextProps,
    get children() {
      return createComponent(
        FormControlLabelRoot,
        mergeProps(otherProps, {
          get ["class"]() {
            return clsx(classes.root, otherProps.class);
          },
          ownerState: ownerState,
          get children() {
            return [
              createMemo(() => props.control),
              createComponent(Label, {}),
            ];
          },
        }),
      );
    },
  });
});

function getFormGroupUtilityClass(slot) {
  return generateUtilityClass("MuiFormGroup", slot);
}
generateUtilityClasses("MuiFormGroup", ["root", "row", "error"]);

const $$J = createComponentFactory()({
  name: "MuiFormGroup",
  propDefaults: ({ set }) =>
    set({
      row: false,
    }),
  selfPropNames: ["children", "classes", "row"],
  utilityClass: getFormGroupUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", ownerState.row && "row", !!ownerState.error && "error"],
  }),
});
const FormGroupRoot = styled$1("div", {
  name: "MuiFormGroup",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.root, ownerState.row && styles.row];
  },
})(({ ownerState }) => ({
  display: "flex",
  flexDirection: "column",
  flexWrap: "wrap",
  ...(ownerState.row && {
    flexDirection: "row",
  }),
}));

/**
 * `FormGroup` wraps controls such as `Checkbox` and `Switch`.
 * It provides compact row layout.
 * For the `Radio`, you should be using the `RadioGroup` component instead of this one.
 *
 * Demos:
 *
 * - [Checkboxes](https://mui.com/components/checkboxes/)
 * - [Switches](https://mui.com/components/switches/)
 *
 * API:
 *
 * - [FormGroup API](https://mui.com/api/form-group/)
 */
const FormGroup = $$J.component(function FormGroup({
  props,
  allProps,
  classes,
  otherProps,
}) {
  const muiFormControl = useFormControl();
  const fcs = formControlState({
    props: allProps,
    muiFormControl,
    states: ["error"],
  });
  const ownerState = mergeProps(allProps, {
    get error() {
      return fcs.error;
    },
  });
  return createComponent(
    FormGroupRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: ownerState,
      get children() {
        return props.children;
      },
    }),
  );
});

function getFormHelperTextUtilityClasses(slot) {
  return generateUtilityClass("MuiFormHelperText", slot);
}
const formHelperTextClasses = generateUtilityClasses("MuiFormHelperText", [
  "root",
  "error",
  "disabled",
  "sizeSmall",
  "sizeMedium",
  "contained",
  "focused",
  "filled",
  "required",
]);

const _tmpl$$7 = /*#__PURE__*/ template(`<span class="notranslate">&#8203;`);
const $$I = createComponentFactory()({
  name: "MuiFormHelperText",
  propDefaults: ({ set }) =>
    set({
      component: "p",
    }),
  selfPropNames: [
    "children",
    "classes",
    "disabled",
    "error",
    "filled",
    "focused",
    "margin",
    "required",
    "variant",
  ],
  utilityClass: getFormHelperTextUtilityClasses,
  autoCallUseClasses: false,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !!ownerState.disabled && "disabled",
      !!ownerState.error && "error",
      !!ownerState.size && `size${capitalize(ownerState.size)}`,
      ownerState.contained && "contained",
      !!ownerState.focused && "focused",
      !!ownerState.filled && "filled",
      !!ownerState.required && "required",
    ],
  }),
});
const FormHelperTextRoot = styled$1("p", {
  name: "MuiFormHelperText",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.size && styles[`size${capitalize(ownerState.size)}`],
      ownerState.contained && styles.contained,
      ownerState.filled && styles.filled,
    ];
  },
})(({ theme, ownerState }) => ({
  color: theme.palette.text.secondary,
  ...theme.typography.caption,
  textAlign: "left",
  marginTop: 3,
  marginRight: 0,
  marginBottom: 0,
  marginLeft: 0,
  [`&.${formHelperTextClasses.disabled}`]: {
    color: theme.palette.text.disabled,
  },
  [`&.${formHelperTextClasses.error}`]: {
    color: theme.palette.error.main,
  },
  ...(ownerState.size === "small" && {
    marginTop: 4,
  }),
  ...(ownerState.contained && {
    marginLeft: 14,
    marginRight: 14,
  }),
}));
const FormHelperText = $$I.component(function FormHelperText({
  allProps,
  otherProps,
  props,
}) {
  const muiFormControl = useFormControl();
  const fcs = formControlState({
    props: allProps,
    muiFormControl,
    states: [
      "variant",
      "size",
      "disabled",
      "error",
      "filled",
      "focused",
      "required",
    ],
  });
  const ownerState = mergeProps(allProps, {
    get contained() {
      return fcs.variant === "filled" || fcs.variant === "outlined";
    },
    get variant() {
      return fcs.variant;
    },
    get size() {
      return fcs.size;
    },
    get disabled() {
      return fcs.disabled;
    },
    get error() {
      return fcs.error;
    },
    get filled() {
      return fcs.filled;
    },
    get focused() {
      return fcs.focused;
    },
    get required() {
      return fcs.required;
    },
  });
  const classes = $$I.useClasses(ownerState);
  const resolved = children(() => props.children);
  return createComponent(
    FormHelperTextRoot,
    mergeProps(otherProps, {
      get as() {
        return otherProps.component;
      },
      ownerState: ownerState,
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      get children() {
        return createMemo(() => resolved() === " ")() // notranslate needed while Google Translate will not fix zero-width space issue
          ? _tmpl$$7()
          : resolved();
      },
    }),
  );
});

function getFormLabelUtilityClasses(slot) {
  return generateUtilityClass("MuiFormLabel", slot);
}
const formLabelClasses = generateUtilityClasses("MuiFormLabel", [
  "root",
  "colorSecondary",
  "focused",
  "disabled",
  "error",
  "filled",
  "required",
  "asterisk",
]);

const $$H = createComponentFactory()({
  name: "MuiFormLabel",
  propDefaults: ({ set }) =>
    set({
      component: "label",
    }),
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disabled",
    "error",
    "filled",
    "focused",
    "required",
  ],
  autoCallUseClasses: false,
  utilityClass: getFormLabelUtilityClasses,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      `color${capitalize(ownerState.color)}`,
      !!ownerState.disabled && "disabled",
      !!ownerState.error && "error",
      !!ownerState.filled && "filled",
      !!ownerState.focused && "focused",
      !!ownerState.required && "required",
    ],
    asterisk: ["asterisk", !!ownerState.error && "error"],
  }),
});
const FormLabelRoot = styled$1("label", {
  name: "MuiFormLabel",
  slot: "Root",
  overridesResolver: ({ ownerState }, styles) => {
    return {
      ...styles.root,
      ...(ownerState.color === "secondary" && styles.colorSecondary),
      ...(ownerState.filled && styles.filled),
    };
  },
})(({ theme, ownerState }) => ({
  color: theme.palette.text.secondary,
  ...theme.typography.body1,
  lineHeight: "1.4375em",
  padding: 0,
  position: "relative",
  [`&.${formLabelClasses.focused}`]: {
    color: theme.palette[ownerState.color].main,
  },
  [`&.${formLabelClasses.disabled}`]: {
    color: theme.palette.text.disabled,
  },
  [`&.${formLabelClasses.error}`]: {
    color: theme.palette.error.main,
  },
}));
const AsteriskComponent = styled$1("span", {
  name: "MuiFormLabel",
  slot: "Asterisk",
  overridesResolver: (props, styles) => styles.asterisk,
})(({ theme }) => ({
  [`&.${formLabelClasses.error}`]: {
    color: theme.palette.error.main,
  },
}));
const FormLabel = $$H.component(function FormLabel({
  allProps,
  otherProps,
  props,
}) {
  const muiFormControl = useFormControl();
  const fcs = formControlState({
    props: allProps,
    muiFormControl,
    states: ["color", "required", "focused", "disabled", "error", "filled"],
  });
  const ownerState = mergeProps(allProps, {
    get color() {
      return fcs.color || "primary";
    },
    get disabled() {
      return fcs.disabled;
    },
    get error() {
      return fcs.error;
    },
    get filled() {
      return fcs.filled;
    },
    get focused() {
      return fcs.focused;
    },
    get required() {
      return fcs.required;
    },
  });
  const classes = $$H.useClasses(ownerState);
  return createComponent(
    FormLabelRoot,
    mergeProps(otherProps, {
      get as() {
        return otherProps.component;
      },
      ownerState: ownerState,
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      get children() {
        return [
          createMemo(() => props.children),
          createComponent(Show, {
            get when() {
              return fcs.required;
            },
            get children() {
              return createComponent(AsteriskComponent, {
                ownerState: ownerState,
                "aria-hidden": true,
                get ["class"]() {
                  return classes.asterisk;
                },
                get children() {
                  return ["\u2009", "*"];
                },
              });
            },
          }),
        ];
      },
    }),
  );
});

const GridContext = createContext();

function getGridUtilityClass(slot) {
  return generateUtilityClass("MuiGrid", slot);
}
const SPACINGS = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const DIRECTIONS = ["column-reverse", "column", "row-reverse", "row"];
const WRAPS = ["nowrap", "wrap-reverse", "wrap"];
const GRID_SIZES = ["auto", true, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
const gridClasses = generateUtilityClasses("MuiGrid", [
  "root",
  "container",
  "item",
  "zeroMinWidth",
  // spacings
  ...SPACINGS.map((spacing) => `spacing-xs-${spacing}`),
  // direction values
  ...DIRECTIONS.map((direction) => `direction-xs-${direction}`),
  // wrap values
  ...WRAPS.map((wrap) => `wrap-xs-${wrap}`),
  // grid sizes for all breakpoints
  ...GRID_SIZES.map((size) => `grid-xs-${size}`),
  ...GRID_SIZES.map((size) => `grid-sm-${size}`),
  ...GRID_SIZES.map((size) => `grid-md-${size}`),
  ...GRID_SIZES.map((size) => `grid-lg-${size}`),
  ...GRID_SIZES.map((size) => `grid-xl-${size}`),
]);

const $$G = createComponentFactory()({
  name: "MuiGrid",
  selfPropNames: [
    "children",
    "classes",
    "columnSpacing",
    "columns",
    "container",
    "direction",
    "item",
    "lg",
    "md",
    "rowSpacing",
    "sm",
    "spacing",
    "wrap",
    "xl",
    "xs",
    "zeroMinWidth",
  ],
  propDefaults: ({ set, inProps }) => {
    const columnsContext = useContext(GridContext);
    return set({
      get columns() {
        return inProps.columns || columnsContext || 12;
      },
      component: "div",
      container: false,
      direction: "row",
      item: false,
      lg: false,
      md: false,
      sm: false,
      spacing: 0,
      wrap: "wrap",
      xl: false,
      xs: false,
      zeroMinWidth: false,
      get rowSpacing() {
        return inProps.rowSpacing ?? inProps.spacing ?? 0;
      },
      get columnSpacing() {
        return inProps.columnSpacing ?? inProps.spacing ?? 0;
      },
    });
  },
  utilityClass: getGridUtilityClass,
  slotClasses: (o) => ({
    root: [
      "root",
      o.container && "container",
      o.item && "item",
      o.zeroMinWidth && "zeroMinWidth",
      ...resolveSpacingClasses(o.spacing, o.container),
      o.direction !== "row" && `direction-xs-${String(o.direction)}`,
      o.wrap !== "wrap" && `wrap-xs-${String(o.wrap)}`,
      o.xs !== false && `grid-xs-${String(o.xs)}`,
      o.sm !== false && `grid-sm-${String(o.sm)}`,
      o.md !== false && `grid-md-${String(o.md)}`,
      o.lg !== false && `grid-lg-${String(o.lg)}`,
      o.xl !== false && `grid-xl-${String(o.xl)}`,
    ],
  }),
});
function getOffset(val) {
  const parse = parseFloat(val);
  return `${parse}${String(val).replace(String(parse), "") || "px"}`;
}
function generateGrid(input) {
  const { theme, ownerState } = input;
  let size;
  return theme.breakpoints.keys.reduce((globalStyles, breakpoint) => {
    // Use side effect over immutability for better performance.
    let styles = {};
    if (ownerState[breakpoint]) {
      size = ownerState[breakpoint];
    }
    if (!size) {
      return globalStyles;
    }
    if (size === true) {
      // For the auto layouting
      styles = {
        flexBasis: 0,
        flexGrow: 1,
        maxWidth: "100%",
      };
    } else if (size === "auto") {
      styles = {
        flexBasis: "auto",
        flexGrow: 0,
        flexShrink: 0,
        maxWidth: "none",
        width: "auto",
      };
    } else {
      const columnsBreakpointValues = resolveBreakpointValues({
        values: ownerState.columns,
        breakpoints: theme.breakpoints.values,
      });
      const columnValue =
        typeof columnsBreakpointValues === "object"
          ? columnsBreakpointValues[breakpoint]
          : columnsBreakpointValues;
      if (columnValue === undefined || columnValue === null) {
        return globalStyles;
      }
      // Keep 7 significant numbers.
      const width = `${Math.round((size / columnValue) * 10e7) / 10e5}%`;
      let more = {};
      if (
        ownerState.container &&
        ownerState.item &&
        ownerState.columnSpacing !== 0
      ) {
        const themeSpacing = theme.spacing(ownerState.columnSpacing);
        if (themeSpacing !== "0px") {
          const fullWidth = `calc(${width} + ${getOffset(themeSpacing)})`;
          more = {
            flexBasis: fullWidth,
            maxWidth: fullWidth,
          };
        }
      }

      // Close to the bootstrap implementation:
      // https://github.com/twbs/bootstrap/blob/8fccaa2439e97ec72a4b7dc42ccc1f649790adb0/scss/mixins/_grid.scss#L41
      styles = {
        flexBasis: width,
        flexGrow: 0,
        maxWidth: width,
        ...more,
      };
    }

    // No need for a media query for the first size.
    if (theme.breakpoints.values[breakpoint] === 0) {
      Object.assign(globalStyles, styles);
    } else {
      globalStyles = {
        ...globalStyles,
        ...{
          [theme.breakpoints.up(breakpoint)]: styles,
        },
      };
    }
    return globalStyles;
  }, {});
}
function generateDirection(input) {
  const { theme, ownerState } = input;
  const directionValues = resolveBreakpointValues({
    values: ownerState.direction,
    breakpoints: theme.breakpoints.values,
  });
  return handleBreakpoints(
    {
      theme,
    },
    directionValues,
    (propValue) => {
      let output = {
        flexDirection: propValue,
      };
      if (propValue.indexOf("column") === 0) {
        output = {
          ...output,
          ...{
            [`& > .${gridClasses.item}`]: {
              maxWidth: "none",
            },
          },
        };
      }
      return output;
    },
  );
}
function generateRowGap(input) {
  const { theme, ownerState } = input;
  const { container, rowSpacing } = ownerState;
  let styles = {};
  if (container && rowSpacing !== 0) {
    const rowSpacingValues = resolveBreakpointValues({
      values: rowSpacing,
      breakpoints: theme.breakpoints.values,
    });
    styles = handleBreakpoints(
      {
        theme,
      },
      rowSpacingValues,
      (propValue) => {
        const themeSpacing = theme.spacing(propValue);
        if (themeSpacing !== "0px") {
          return {
            marginTop: `-${getOffset(themeSpacing)}`,
            [`& > .${gridClasses.item}`]: {
              paddingTop: getOffset(themeSpacing),
            },
          };
        }
        return {};
      },
    );
  }
  return styles;
}
function generateColumnGap(input) {
  const { theme, ownerState } = input;
  const { container, columnSpacing } = ownerState;
  let styles = {};
  if (container && columnSpacing !== 0) {
    const columnSpacingValues = resolveBreakpointValues({
      values: columnSpacing,
      breakpoints: theme.breakpoints.values,
    });
    styles = handleBreakpoints(
      {
        theme,
      },
      columnSpacingValues,
      (propValue) => {
        const themeSpacing = theme.spacing(propValue);
        if (themeSpacing !== "0px") {
          return {
            width: `calc(100% + ${getOffset(themeSpacing)})`,
            marginLeft: `-${getOffset(themeSpacing)}`,
            [`& > .${gridClasses.item}`]: {
              paddingLeft: getOffset(themeSpacing),
            },
          };
        }
        return {};
      },
    );
  }
  return styles;
}
function resolveSpacingClasses(spacing, container, styles = {}) {
  // in case of grid item or undefined/null or `spacing` <= 0
  if (!container || !spacing || spacing <= 0) {
    return [];
  }
  // in case of string/number `spacing`
  if (
    (typeof spacing === "string" && !Number.isNaN(Number(spacing))) ||
    typeof spacing === "number"
  ) {
    return [
      styles[`spacing-xs-${String(spacing)}`] ||
        `spacing-xs-${String(spacing)}`,
    ];
  } else if (typeof spacing === "string" || Array.isArray(spacing)) {
    return [];
  }
  // in case of object `spacing`
  const xs = spacing.xs;
  const sm = spacing.sm;
  const md = spacing.md;
  const lg = spacing.lg;
  const xl = spacing.xl;
  return [
    Number(xs) > 0 &&
      (styles[`spacing-xs-${String(xs)}`] || `spacing-xs-${String(xs)}`),
    Number(sm) > 0 &&
      (styles[`spacing-sm-${String(sm)}`] || `spacing-sm-${String(sm)}`),
    Number(md) > 0 &&
      (styles[`spacing-md-${String(md)}`] || `spacing-md-${String(md)}`),
    Number(lg) > 0 &&
      (styles[`spacing-lg-${String(lg)}`] || `spacing-lg-${String(lg)}`),
    Number(xl) > 0 &&
      (styles[`spacing-xl-${String(xl)}`] || `spacing-xl-${String(xl)}`),
  ];
}

// Default CSS values
// flex: '0 1 auto',
// flexDirection: 'row',
// alignItems: 'flex-start',
// flexWrap: 'nowrap',
// justifyContent: 'flex-start',

const GridRoot = styled$1("div", {
  name: "MuiGrid",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const {
      container,
      direction,
      item,
      lg,
      md,
      sm,
      spacing,
      wrap,
      xl,
      xs,
      zeroMinWidth,
    } = props.ownerState;
    return [
      styles.root,
      container && styles.container,
      item && styles.item,
      zeroMinWidth && styles.zeroMinWidth,
      ...resolveSpacingClasses(spacing, container, styles),
      direction !== "row" && styles[`direction-xs-${String(direction)}`],
      wrap !== "wrap" && styles[`wrap-xs-${String(wrap)}`],
      xs !== false && styles[`grid-xs-${String(xs)}`],
      sm !== false && styles[`grid-sm-${String(sm)}`],
      md !== false && styles[`grid-md-${String(md)}`],
      lg !== false && styles[`grid-lg-${String(lg)}`],
      xl !== false && styles[`grid-xl-${String(xl)}`],
    ];
  },
})(
  ({ ownerState }) => ({
    boxSizing: "border-box",
    ...(ownerState.container && {
      display: "flex",
      flexWrap: "wrap",
      width: "100%",
    }),
    ...(ownerState.item && {
      margin: 0, // For instance, it's useful when used with a `figure` element.
    }),

    ...(ownerState.zeroMinWidth && {
      minWidth: 0,
    }),
    ...(ownerState.wrap !== "wrap" && {
      flexWrap: ownerState.wrap,
    }),
  }),
  generateDirection,
  generateRowGap,
  generateColumnGap,
  generateGrid,
);

/**
 *
 * Demos:
 *
 * - [Grid](https://mui.com/components/grid/)
 *
 * API:
 *
 * - [Grid API](https://mui.com/api/grid/)
 */
const Grid = $$G.component(function Grid({
  allProps,
  props,
  otherProps,
  classes,
}) {
  otherProps = extendSxProp(otherProps);
  const RootElement = () =>
    createComponent(
      GridRoot,
      mergeProps(
        {
          ownerState: allProps,
        },
        otherProps,
        {
          get ["class"]() {
            return clsx(classes.root, otherProps.class);
          },
          get children() {
            return props.children;
          },
        },
      ),
    );
  return createComponent(Show, {
    get when() {
      return props.columns !== 12;
    },
    get fallback() {
      return createComponent(RootElement, {});
    },
    get children() {
      return createComponent(GridContext.Provider, {
        get value() {
          return props.columns;
        },
        get children() {
          return createComponent(RootElement, {});
        },
      });
    },
  });
});

const $$F = createComponentFactory()({
  name: "MuiGrow",
  propDefaults: ({ set }) =>
    set({
      appear: true,
      timeout: "auto",
    }),
  selfPropNames: ["appear", "children", "easing", "in", "ref", "timeout"],
});
function getScale(value) {
  return `scale(${value}, ${value ** 2})`;
}
const styles = {
  entering: {
    opacity: 1,
    transform: getScale(1),
  },
  entered: {
    opacity: 1,
    transform: "none",
  },
};

/**
 * The Grow transition is used by the [Tooltip](/components/tooltips/) and
 * [Popover](/components/popover/) components.
 * It uses [react-transition-group](https://github.com/reactjs/react-transition-group) internally.
 */
const Grow = $$F.component(function Grow({ props, otherProps }) {
  const autoTimeout = {
    current: undefined,
  };
  const theme = useTheme$1();
  const resolved = children(() => props.children);
  let timer;
  onCleanup(() => timer && clearTimeout(timer));
  return createComponent(
    Transition,
    mergeProps(otherProps, {
      get appear() {
        return props.appear;
      },
      get ["in"]() {
        return props.in;
      },
      onEnter: () => {
        const node = resolved();
        reflow(node); // So the animation always start from the start.

        const {
          duration: transitionDuration,
          delay,
          easing: transitionTimingFunction,
        } = getTransitionProps(
          {
            style: otherProps.style,
            timeout: props.timeout,
            easing: props.easing,
          },
          {
            mode: "enter",
          },
        );
        let duration;
        if (props.timeout === "auto") {
          duration = theme.transitions.getAutoHeightDuration(node.clientHeight);
          autoTimeout.current = duration;
        } else {
          duration = transitionDuration;
        }
        node.style.transition = [
          theme.transitions.create("opacity", {
            duration,
            delay,
          }),
          theme.transitions.create("transform", {
            duration: Number(duration) * 0.666,
            delay,
            easing: transitionTimingFunction,
          }),
        ].join(",");
        otherProps.onEnter?.();
      },
      onExit: () => {
        const node = resolved();
        const {
          duration: transitionDuration,
          delay,
          easing: transitionTimingFunction,
        } = getTransitionProps(
          {
            style: otherProps.style,
            timeout: props.timeout,
            easing: props.easing,
          },
          {
            mode: "exit",
          },
        );
        let duration;
        if (props.timeout === "auto") {
          duration = theme.transitions.getAutoHeightDuration(node.clientHeight);
          autoTimeout.current = duration;
        } else {
          duration = transitionDuration;
        }
        node.style.transition = [
          theme.transitions.create("opacity", {
            duration,
            delay,
          }),
          theme.transitions.create("transform", {
            duration: Number(duration) * 0.666,
            delay: delay || Number(duration) * 0.333,
            easing: transitionTimingFunction,
          }),
        ].join(",");
        node.style.opacity = "0";
        node.style.transform = getScale(0.75);
        otherProps.onExit?.();
      },
      addEndListener: (next) => {
        if (props.timeout === "auto") {
          timer = setTimeout(next, autoTimeout.current || 0);
        }
        if (otherProps.addEndListener) {
          otherProps.addEndListener(next);
        }
      },
      get timeout() {
        return props.timeout === "auto" ? undefined : props.timeout;
      },
      children: (state) => {
        const element = resolved();
        element.style.opacity = "0";
        element.style.transform = getScale(0.75);
        if (state === "exited" && !props.in) {
          element.style.visibility = "hidden";
        } else {
          element.style.removeProperty("visibility");
        }
        const style = {
          ...(styles[state] || {}),
          ...(otherProps.style || {}),
        };
        for (const name in style) {
          const value = style[name];
          if (value === undefined) {
            element.style.removeProperty(name);
          } else {
            element.style[name] = value;
          }
        }
        return element;
      },
    }),
  );
});

function getIconUtilityClass(slot) {
  return generateUtilityClass("MuiIcon", slot);
}
generateUtilityClasses("MuiIcon", [
  "root",
  "colorPrimary",
  "colorSecondary",
  "colorAction",
  "colorError",
  "colorDisabled",
  "fontSizeInherit",
  "fontSizeSmall",
  "fontSizeMedium",
  "fontSizeLarge",
]);

const $$E = createComponentFactory()({
  name: "MuiIcon",
  selfPropNames: ["baseClassName", "children", "classes", "color", "fontSize"],
  propDefaults: ({ set }) =>
    set({
      baseClassName: "material-icons",
      color: "inherit",
      component: "span",
      fontSize: "medium",
    }),
  utilityClass: getIconUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.color !== "inherit" && `color${capitalize(ownerState.color)}`,
      `fontSize${capitalize(ownerState.fontSize)}`,
    ],
  }),
});
const IconRoot = styled$1("span", {
  name: "MuiIcon",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.color !== "inherit" &&
        styles[`color${capitalize(ownerState.color)}`],
      styles[`fontSize${capitalize(ownerState.fontSize)}`],
    ];
  },
})(({ theme, ownerState }) => ({
  userSelect: "none",
  width: "1em",
  height: "1em",
  // Chrome fix for https://bugs.chromium.org/p/chromium/issues/detail?id=820541
  // To remove at some point.
  overflow: "hidden",
  display: "inline-block",
  // allow overflow hidden to take action
  textAlign: "center",
  // support non-square icon
  flexShrink: 0,
  fontSize: {
    inherit: "inherit",
    small: theme.typography.pxToRem(20),
    medium: theme.typography.pxToRem(24),
    large: theme.typography.pxToRem(36),
  }[ownerState.fontSize],
  // TODO v5 deprecate, v6 remove for sx
  color: {
    primary: theme.palette.primary.main,
    secondary: theme.palette.secondary.main,
    info: theme.palette.info.main,
    success: theme.palette.success.main,
    warning: theme.palette.warning.main,
    action: theme.palette.action.active,
    error: theme.palette.error.main,
    disabled: theme.palette.action.disabled,
    inherit: undefined,
  }[ownerState.color],
}));

/**
 *
 * Demos:
 *
 * - [Icons](https://mui.com/components/icons/)
 * - [Material Icons](https://mui.com/components/material-icons/)
 *
 * API:
 *
 * - [Icon API](https://mui.com/api/icon/)
 */
const Icon = $$E.component(function Icon({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    IconRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(
            props.baseClassName,
            // Prevent the translation of the text content.
            // The font relies on the exact text content to render the icon.
            "notranslate",
            classes.root,
            otherProps.class,
          );
        },
        ownerState: allProps,
        "aria-hidden": true,
        get children() {
          return props.children;
        },
      },
      otherProps,
    ),
  );
});

function getInputUtilityClass(slot) {
  return generateUtilityClass("MuiInput", slot);
}
const inputClasses = {
  ...inputBaseClasses,
  ...generateUtilityClasses("MuiInput", ["root", "underline", "input"]),
};

const $$D = createComponentFactory()({
  name: "MuiInput",
  propDefaults: ({ set }) =>
    set({
      components: {},
      fullWidth: false,
      inputComponent: "input",
      multiline: false,
      type: "text",
    }),
  selfPropNames: ["classes", "disableUnderline"],
  utilityClass: getInputUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", !ownerState.disableUnderline && "underline"],
    input: ["input"],
  }),
});
const InputRoot = styled$1(InputBaseRoot, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiInput",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      ...rootOverridesResolver(props, styles),
      !ownerState.disableUnderline && styles.underline,
    ];
  },
})(({ theme, ownerState }) => {
  const light = theme.palette.mode === "light";
  const bottomLineColor = light
    ? "rgba(0, 0, 0, 0.42)"
    : "rgba(255, 255, 255, 0.7)";
  return {
    position: "relative",
    ...(ownerState.formControl && {
      "label + &": {
        marginTop: 16,
      },
    }),
    ...(!ownerState.disableUnderline && {
      "&:after": {
        borderBottom: `2px solid ${theme.palette[ownerState.color].main}`,
        left: 0,
        bottom: 0,
        // Doing the other way around crash on IE11 "''" https://github.com/cssinjs/jss/issues/242
        content: '""',
        position: "absolute",
        right: 0,
        transform: "scaleX(0)",
        transition: theme.transitions.create("transform", {
          duration: theme.transitions.duration.shorter,
          easing: theme.transitions.easing.easeOut,
        }),
        pointerEvents: "none", // Transparent to the hover style.
      },

      [`&.${inputClasses.focused}:after`]: {
        transform: "scaleX(1)",
      },
      [`&.${inputClasses.error}:after`]: {
        borderBottomColor: theme.palette.error.main,
        transform: "scaleX(1)", // error is always underlined in red
      },

      "&:before": {
        borderBottom: `1px solid ${bottomLineColor}`,
        left: 0,
        bottom: 0,
        // Doing the other way around crash on IE11 "''" https://github.com/cssinjs/jss/issues/242
        content: '"\\00a0"',
        position: "absolute",
        right: 0,
        transition: theme.transitions.create("border-bottom-color", {
          duration: theme.transitions.duration.shorter,
        }),
        pointerEvents: "none", // Transparent to the hover style.
      },

      [`&:hover:not(.${inputClasses.disabled}):before`]: {
        borderBottom: `2px solid ${theme.palette.text.primary}`,
        // Reset on touch devices, it doesn't add specificity
        "@media (hover: none)": {
          borderBottom: `1px solid ${bottomLineColor}`,
        },
      },
      [`&.${inputClasses.disabled}:before`]: {
        borderBottomStyle: "dotted",
      },
    }),
  };
});
const InputInput = styled$1(InputBaseComponent, {
  name: "MuiInput",
  slot: "Input",
  overridesResolver: inputOverridesResolver,
})({});
const Input = $$D.component(function Input({ classes, otherProps, props }) {
  const componentsProps = createMemo(() => {
    const ownerState = {
      disableUnderline: props.disableUnderline,
    };
    const inputComponentsProps = {
      root: {
        ownerState,
      },
    };
    return otherProps.componentsProps
      ? deepmerge(otherProps.componentsProps, inputComponentsProps)
      : inputComponentsProps;
  });
  const allClasses = mergeProps(classes, () => props.classes || {});
  return createComponent(
    InputBase,
    mergeProps(otherProps, {
      get components() {
        return {
          Root: InputRoot,
          Input: InputInput,
          ...(otherProps.components || {}),
        };
      },
      get componentsProps() {
        return componentsProps();
      },
      classes: allClasses,
    }),
  );
});

function getInputAdornmentUtilityClass(slot) {
  return generateUtilityClass("MuiInputAdornment", slot);
}
const inputAdornmentClasses = generateUtilityClasses("MuiInputAdornment", [
  "root",
  "filled",
  "standard",
  "outlined",
  "positionStart",
  "positionEnd",
  "disablePointerEvents",
  "hiddenLabel",
  "sizeSmall",
]);

const _tmpl$$6 = /*#__PURE__*/ template(`<span class="notranslate">&#8203;`);
const $$C = createComponentFactory()({
  name: "MuiInputAdornment",
  selfPropNames: [
    "classes",
    "children",
    "disablePointerEvents",
    "disableTypography",
    "position",
    "variant",
  ],
  utilityClass: getInputAdornmentUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.disablePointerEvents && "disablePointerEvents",
      ownerState.position && `position${capitalize(ownerState.position)}`,
      ownerState.variant,
      ownerState.hiddenLabel && "hiddenLabel",
      ownerState.size && `size${capitalize(ownerState.size)}`,
    ],
  }),
});
const InputAdornmentRoot = styled$1("div", {
  name: "MuiInputAdornment",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[`position${capitalize(ownerState.position)}`],
      ownerState.disablePointerEvents === true && styles.disablePointerEvents,
      styles[ownerState.variant],
    ];
  },
})(({ theme, ownerState }) => ({
  display: "flex",
  height: "0.01em",
  // Fix IE11 flexbox alignment. To remove at some point.
  maxHeight: "2em",
  alignItems: "center",
  whiteSpace: "nowrap",
  color: theme.palette.action.active,
  ...(ownerState.variant === "filled" && {
    // Styles applied to the root element if `variant="filled"`.
    [`&.${inputAdornmentClasses.positionStart}&:not(.${inputAdornmentClasses.hiddenLabel})`]:
      {
        marginTop: 16,
      },
  }),
  ...(ownerState.position === "start" && {
    // Styles applied to the root element if `position="start"`.
    marginRight: 8,
  }),
  ...(ownerState.position === "end" && {
    // Styles applied to the root element if `position="end"`.
    marginLeft: 8,
  }),
  ...(ownerState.disablePointerEvents === true && {
    // Styles applied to the root element if `disablePointerEvents={true}`.
    pointerEvents: "none",
  }),
}));

/**
 *
 * Demos:
 *
 * - [Text Fields](https://mui.com/components/text-fields/)
 *
 * API:
 *
 * - [InputAdornment API](https://mui.com/api/input-adornment/)
 */
const InputAdornment = $$C.defineComponent(function InputAdornment(inProps) {
  const ref = createRef(inProps);
  const props = $$C.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "children",
    "class",
    "component",
    "disablePointerEvents",
    "disableTypography",
    "position",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      component: "div",
      disablePointerEvents: false,
      disableTypography: false,
    },
    props,
  );
  const muiFormControl = useFormControl();
  const variant = () => {
    let variant = props.variant;
    if (variant && muiFormControl?.variant);
    if (muiFormControl && !variant) {
      variant = muiFormControl.variant;
    }
    return variant;
  };
  const ownerState = {
    get hiddenLabel() {
      return muiFormControl?.hiddenLabel;
    },
    get size() {
      return muiFormControl?.size;
    },
    get disablePointerEvents() {
      return baseProps.disablePointerEvents;
    },
    get position() {
      return props.position;
    },
    get variant() {
      return variant();
    },
  };
  const classes = $$C.useClasses(ownerState);
  function RootChildren(props) {
    const c = children(() => props.children);
    return createComponent(Show, {
      get when() {
        return typeof c() === "string" && !props.disableTypography;
      },
      get fallback() {
        return [
          createMemo(
            (() => {
              const _c$ = createMemo(() => props.position === "start");
              return () =>
                _c$()
                  ? /* notranslate needed while Google Translate will not fix zero-width space issue */ _tmpl$$6()
                  : null;
            })(),
          ),
          createMemo(() => props.children),
        ];
      },
      get children() {
        return createComponent(Typography, {
          color: "text.secondary",
          get children() {
            return c();
          },
        });
      },
    });
  }
  return createComponent(FormControlContext.Provider, {
    value: undefined,
    get children() {
      return createComponent(
        InputAdornmentRoot,
        mergeProps(
          {
            get as() {
              return baseProps.component;
            },
            ownerState: ownerState,
            get ["class"]() {
              return clsx(classes.root, props.class);
            },
            ref: ref,
          },
          other,
          {
            get children() {
              return createComponent(RootChildren, {
                get position() {
                  return props.position;
                },
                get disableTypography() {
                  return baseProps.disableTypography;
                },
                get children() {
                  return props.children;
                },
              });
            },
          },
        ),
      );
    },
  });
});

function getInputLabelUtilityClasses(slot) {
  return generateUtilityClass("MuiInputLabel", slot);
}
generateUtilityClasses("MuiInputLabel", [
  "root",
  "focused",
  "disabled",
  "error",
  "required",
  "asterisk",
  "formControl",
  "sizeSmall",
  "shrink",
  "animated",
  "standard",
  "filled",
  "outlined",
]);

const $$B = createComponentFactory()({
  name: "MuiInputLabel",
  propDefaults: ({ set }) =>
    set({
      disableAnimation: false,
    }),
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disableAnimation",
    "disabled",
    "error",
    "focused",
    "margin",
    "required",
    "shrink",
    "variant",
  ],
  autoCallUseClasses: false,
  utilityClass: getInputLabelUtilityClasses,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !!ownerState.formControl && "formControl",
      !ownerState.disableAnimation && "animated",
      !!ownerState.shrink && "shrink",
      ownerState.size === "small" && "sizeSmall",
      !!ownerState.variant && ownerState.variant,
    ],
    asterisk: [!!ownerState.required && "asterisk"],
  }),
});
const InputLabelRoot = styled$1(FormLabel, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiInputLabel",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      {
        [`& .${formLabelClasses.asterisk}`]: styles.asterisk,
      },
      styles.root,
      ownerState.formControl && styles.formControl,
      ownerState.size === "small" && styles.sizeSmall,
      ownerState.shrink && styles.shrink,
      !ownerState.disableAnimation && styles.animated,
      styles[ownerState.variant],
    ];
  },
})(({ theme, ownerState }) => ({
  display: "block",
  transformOrigin: "top left",
  whiteSpace: "nowrap",
  overflow: "hidden",
  textOverflow: "ellipsis",
  maxWidth: "100%",
  ...(ownerState.formControl && {
    position: "absolute",
    left: 0,
    top: 0,
    // slight alteration to spec spacing to match visual spec result
    transform: "translate(0, 20px) scale(1)",
  }),
  ...(ownerState.size === "small" && {
    // Compensation for the `Input.inputSizeSmall` style.
    transform: "translate(0, 17px) scale(1)",
  }),
  ...(ownerState.shrink && {
    transform: "translate(0, -1.5px) scale(0.75)",
    transformOrigin: "top left",
    maxWidth: "133%",
  }),
  ...(!ownerState.disableAnimation && {
    transition: theme.transitions.create(["color", "transform", "max-width"], {
      duration: theme.transitions.duration.shorter,
      easing: theme.transitions.easing.easeOut,
    }),
  }),
  ...(ownerState.variant === "filled" && {
    // Chrome's autofill feature gives the input field a yellow background.
    // Since the input field is behind the label in the HTML tree,
    // the input field is drawn last and hides the label with an opaque background color.
    // zIndex: 1 will raise the label above opaque background-colors of input.
    zIndex: 1,
    pointerEvents: "none",
    transform: "translate(12px, 16px) scale(1)",
    maxWidth: "calc(100% - 24px)",
    ...(ownerState.size === "small" && {
      transform: "translate(12px, 13px) scale(1)",
    }),
    ...(ownerState.shrink && {
      userSelect: "none",
      pointerEvents: "auto",
      transform: "translate(12px, 7px) scale(0.75)",
      maxWidth: "calc(133% - 24px)",
      ...(ownerState.size === "small" && {
        transform: "translate(12px, 4px) scale(0.75)",
      }),
    }),
  }),
  ...(ownerState.variant === "outlined" && {
    // see comment above on filled.zIndex
    zIndex: 1,
    pointerEvents: "none",
    transform: "translate(14px, 16px) scale(1)",
    maxWidth: "calc(100% - 24px)",
    ...(ownerState.size === "small" && {
      transform: "translate(14px, 9px) scale(1)",
    }),
    ...(ownerState.shrink && {
      userSelect: "none",
      pointerEvents: "auto",
      maxWidth: "calc(133% - 24px)",
      transform: "translate(14px, -9px) scale(0.75)",
    }),
  }),
}));

/**
 *
 * Demos:
 *
 * - [Text Fields](https://mui.com/components/text-fields/)
 *
 * API:
 *
 * - [InputLabel API](https://mui.com/api/input-label/)
 * - inherits [FormLabel API](https://mui.com/api/form-label/)
 */

const InputLabel = $$B.component(function InputLabel({ allProps, props }) {
  const muiFormControl = useFormControl();
  const [, baseProps] = splitProps(allProps, [
    "disableAnimation",
    "margin",
    "shrink",
    "variant",
  ]);
  const shrink = () => {
    let shrink = props.shrink;
    if (typeof shrink === "undefined" && muiFormControl) {
      shrink =
        muiFormControl.filled ||
        muiFormControl.focused ||
        muiFormControl.adornedStart;
    }
    return shrink;
  };
  const fcs = formControlState({
    props: allProps,
    muiFormControl,
    states: ["size", "variant", "required"],
  });
  const ownerState = mergeProps(allProps, {
    get formControl() {
      return muiFormControl;
    },
    get shrink() {
      return shrink();
    },
    get size() {
      return fcs.size;
    },
    get variant() {
      return fcs.variant;
    },
    get required() {
      return fcs.required;
    },
  });
  const classes = $$B.useClasses(ownerState);
  const allClasses = mergeProps(classes, () => props.classes || {});
  return createComponent(
    InputLabelRoot,
    mergeProps(baseProps, {
      get ["data-shrink"]() {
        return shrink();
      },
      ownerState: ownerState,
      classes: allClasses,
    }),
  );
});

function getLinearProgressUtilityClass(slot) {
  return generateUtilityClass("MuiLinearProgress", slot);
}
generateUtilityClasses("MuiLinearProgress", [
  "root",
  "colorPrimary",
  "colorSecondary",
  "determinate",
  "indeterminate",
  "buffer",
  "query",
  "dashed",
  "dashedColorPrimary",
  "dashedColorSecondary",
  "bar",
  "barColorPrimary",
  "barColorSecondary",
  "bar1Indeterminate",
  "bar1Determinate",
  "bar1Buffer",
  "bar2Indeterminate",
  "bar2Buffer",
]);

const $$A = createComponentFactory()({
  name: "MuiLinearProgress",
  propDefaults: ({ set }) =>
    set({
      color: "primary",
      variant: "indeterminate",
    }),
  selfPropNames: ["classes", "color", "value", "valueBuffer", "variant"],
  utilityClass: getLinearProgressUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", `color${capitalize(ownerState.color)}`, ownerState.variant],
    dashed: ["dashed", `dashedColor${capitalize(ownerState.color)}`],
    bar1: [
      "bar",
      `barColor${capitalize(ownerState.color)}`,
      (ownerState.variant === "indeterminate" ||
        ownerState.variant === "query") &&
        "bar1Indeterminate",
      ownerState.variant === "determinate" && "bar1Determinate",
      ownerState.variant === "buffer" && "bar1Buffer",
    ],
    bar2: [
      "bar",
      ownerState.variant !== "buffer" &&
        `barColor${capitalize(ownerState.color)}`,
      ownerState.variant === "buffer" && `color${capitalize(ownerState.color)}`,
      (ownerState.variant === "indeterminate" ||
        ownerState.variant === "query") &&
        "bar2Indeterminate",
      ownerState.variant === "buffer" && "bar2Buffer",
    ],
  }),
});
const TRANSITION_DURATION = 4; // seconds
const animationId = randomString();
const getColorShade = (theme, color) => {
  if (color === "inherit") {
    return "currentColor";
  }
  return theme.palette.mode === "light"
    ? lighten(theme.palette[color].main, 0.62)
    : darken(theme.palette[color].main, 0.5);
};
const LinearProgressRoot = styled$1("span", {
  name: "MuiLinearProgress",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[`color${capitalize(ownerState.color)}`],
      styles[ownerState.variant],
    ];
  },
})(({ ownerState, theme }) => ({
  position: "relative",
  overflow: "hidden",
  display: "block",
  height: 4,
  zIndex: 0,
  // Fix Safari's bug during composition of different paint.
  "@media print": {
    colorAdjust: "exact",
  },
  backgroundColor: getColorShade(theme, ownerState.color),
  ...(ownerState.color === "inherit" &&
    ownerState.variant !== "buffer" && {
      backgroundColor: "none",
      "&::before": {
        content: '""',
        position: "absolute",
        left: 0,
        top: 0,
        right: 0,
        bottom: 0,
        backgroundColor: "currentColor",
        opacity: 0.3,
      },
    }),
  ...(ownerState.variant === "buffer" && {
    backgroundColor: "transparent",
  }),
  ...(ownerState.variant === "query" && {
    transform: "rotate(180deg)",
  }),
}));
const LinearProgressDashed = styled$1("span", {
  name: "MuiLinearProgress",
  slot: "Dashed",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.dashed,
      styles[`dashedColor${capitalize(ownerState.color)}`],
    ];
  },
})(
  ({ ownerState, theme }) => {
    const backgroundColor = getColorShade(theme, ownerState.color);
    return {
      position: "absolute",
      marginTop: 0,
      height: "100%",
      width: "100%",
      ...(ownerState.color === "inherit" && {
        opacity: 0.3,
      }),
      backgroundImage: `radial-gradient(${backgroundColor} 0%, ${backgroundColor} 16%, transparent 42%)`,
      backgroundSize: "10px 10px",
      backgroundPosition: "0 -23px",
    };
  },
  {
    [`@keyframes buffer-${animationId}`]: {
      0: {
        opacity: 1,
        backgroundPosition: "0 -23px",
      },
      60: {
        opacity: 0,
        backgroundPosition: "0 -23px",
      },
      100: {
        opacity: 1,
        backgroundPosition: "-200px -23px",
      },
    },
    animation: `buffer-${animationId} 3s infinite linear`,
  },
);
const LinearProgressBar1 = styled$1("span", {
  name: "MuiLinearProgress",
  slot: "Bar1",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.bar,
      styles[`barColor${capitalize(ownerState.color)}`],
      (ownerState.variant === "indeterminate" ||
        ownerState.variant === "query") &&
        styles.bar1Indeterminate,
      ownerState.variant === "determinate" && styles.bar1Determinate,
      ownerState.variant === "buffer" && styles.bar1Buffer,
    ];
  },
})(
  ({ ownerState, theme }) => ({
    width: "100%",
    position: "absolute",
    left: 0,
    bottom: 0,
    top: 0,
    transition: "transform 0.2s linear",
    transformOrigin: "left",
    backgroundColor:
      ownerState.color === "inherit"
        ? "currentColor"
        : theme.palette[ownerState.color].main,
    ...(ownerState.variant === "determinate" && {
      transition: `transform .${TRANSITION_DURATION}s linear`,
    }),
    ...(ownerState.variant === "buffer" && {
      zIndex: 1,
      transition: `transform .${TRANSITION_DURATION}s linear`,
    }),
  }),
  ({ ownerState }) =>
    (ownerState.variant === "indeterminate" ||
      ownerState.variant === "query") && {
      [`@keyframes indeterminate1-${animationId}`]: {
        0: {
          left: "-35%",
          right: "100%",
        },
        60: {
          left: "100%",
          right: "-90%",
        },
        100: {
          left: "100%",
          right: "-90%",
        },
      },
      width: "auto",
      animation: `indeterminate1-${animationId} 2.1s cubic-bezier(0.65, 0.815, 0.735, 0.395) infinite`,
    },
);
const LinearProgressBar2 = styled$1("span", {
  name: "MuiLinearProgress",
  slot: "Bar2",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.bar,
      styles[`barColor${capitalize(ownerState.color)}`],
      (ownerState.variant === "indeterminate" ||
        ownerState.variant === "query") &&
        styles.bar2Indeterminate,
      ownerState.variant === "buffer" && styles.bar2Buffer,
    ];
  },
})(
  ({ ownerState, theme }) => ({
    width: "100%",
    position: "absolute",
    left: 0,
    bottom: 0,
    top: 0,
    transition: "transform 0.2s linear",
    transformOrigin: "left",
    ...(ownerState.variant !== "buffer" && {
      backgroundColor:
        ownerState.color === "inherit"
          ? "currentColor"
          : theme.palette[ownerState.color].main,
    }),
    ...(ownerState.color === "inherit" && {
      opacity: 0.3,
    }),
    ...(ownerState.variant === "buffer" && {
      backgroundColor: getColorShade(theme, ownerState.color),
      transition: `transform .${TRANSITION_DURATION}s linear`,
    }),
  }),
  ({ ownerState }) =>
    (ownerState.variant === "indeterminate" ||
      ownerState.variant === "query") && {
      [`@keyframes indeterminate2-${animationId}`]: {
        0: {
          left: "-200%",
          right: "100%",
        },
        60: {
          left: "107%",
          right: "-8%",
        },
        100: {
          left: "107%",
          right: "-8%",
        },
      },
      width: "auto",
      animation: `indeterminate2-${animationId} 2.1s cubic-bezier(0.165, 0.84, 0.44, 1) 1.15s infinite`,
    },
);

/**
 * ## ARIA
 *
 * If the progress bar is describing the loading progress of a particular region of a page,
 * you should use `aria-describedby` to point to the progress bar, and set the `aria-busy`
 * attribute to `true` on that region until it has finished loading.
 *
 * Demos:
 *
 * - [Progress](https://mui.com/components/progress/)
 *
 * API:
 *
 * - [LinearProgress API](https://mui.com/api/linear-progress/)
 */
const LinearProgress = $$A.component(function LinearProgress({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const theme = useTheme$1();
  const element = createElementRef(otherProps);
  const [bar1Style, setBar1Style] = createSignal({});
  const [bar2Style, setBar2Style] = createSignal({});
  createEffect(() => {
    if (props.variant === "determinate" || props.variant === "buffer") {
      if (props.value !== undefined) {
        element.ref.ariaValueNow = Math.round(props.value).toString();
        element.ref.ariaValueMin = "0";
        element.ref.ariaValueMax = "100";
        let transform = props.value - 100;
        if (theme.direction === "rtl") {
          transform = -transform;
        }
        setBar1Style({
          transform: `translateX(${transform}%)`,
        });
      }
    }
  });
  createEffect(() => {
    if (props.variant === "buffer") {
      if (props.valueBuffer !== undefined) {
        let transform = (props.valueBuffer || 0) - 100;
        if (theme.direction === "rtl") {
          transform = -transform;
        }
        setBar2Style({
          transform: `translateX(${transform}%)`,
        });
      }
    }
  });
  const $LinearProgressRoot = redefine(LinearProgressRoot, "span", "div");
  return createComponent(
    $LinearProgressRoot,
    mergeProps(
      {
        role: "progressbar",
      },
      otherProps,
      {
        ref: element,
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        ownerState: allProps,
        get children() {
          return [
            createComponent(Show, {
              get when() {
                return props.variant === "buffer";
              },
              get children() {
                return createComponent(LinearProgressDashed, {
                  get ["class"]() {
                    return classes.dashed;
                  },
                  ownerState: allProps,
                });
              },
            }),
            createComponent(LinearProgressBar1, {
              get ["class"]() {
                return classes.bar1;
              },
              ownerState: allProps,
              get style() {
                return bar1Style();
              },
            }),
            createComponent(Show, {
              get when() {
                return props.variant !== "determinate";
              },
              get children() {
                return createComponent(LinearProgressBar2, {
                  get ["class"]() {
                    return classes.bar2;
                  },
                  ownerState: allProps,
                  get style() {
                    return bar2Style();
                  },
                });
              },
            }),
          ];
        },
      },
    ),
  );
});

function getLinkUtilityClass(slot) {
  return generateUtilityClass("MuiLink", slot);
}
const linkClasses = generateUtilityClasses("MuiLink", [
  "root",
  "underlineNone",
  "underlineHover",
  "underlineAlways",
  "button",
  "focusVisible",
]);

const $$z = createComponentFactory()({
  name: "MuiLink",
  autoCallUseClasses: false,
  selfPropNames: [
    "TypographyClasses",
    "children",
    "classes",
    "color",
    "underline",
    "variant",
  ],
  propDefaults: ({ set }) =>
    set({
      color: "primary",
      component: "a",
      underline: "always",
      variant: "inherit",
    }),
  utilityClass: getLinkUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      `underline${capitalize(ownerState.underline)}`,
      ownerState.component === "button" && "button",
      ownerState.focusVisible && "focusVisible",
    ],
  }),
});
const colorTransformations = {
  primary: "primary.main",
  textPrimary: "text.primary",
  secondary: "secondary.main",
  textSecondary: "text.secondary",
  error: "error.main",
};
const transformDeprecatedColors = (color) => {
  return colorTransformations[color] || color;
};
const LinkRoot = styled$1(Typography, {
  name: "MuiLink",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[`underline${capitalize(ownerState.underline)}`],
      ownerState.component === "button" && styles.button,
    ];
  },
})(({ theme, ownerState }) => {
  const color =
    getThemeValue(
      theme,
      "palette",
      transformDeprecatedColors(ownerState.color),
    ) || ownerState.color;
  return {
    ...(ownerState.underline === "none" && {
      textDecoration: "none",
    }),
    ...(ownerState.underline === "hover" && {
      textDecoration: "none",
      "&:hover": {
        textDecoration: "underline",
      },
    }),
    ...(ownerState.underline === "always" && {
      textDecoration: "underline",
      textDecorationColor: color !== "inherit" ? alpha(color, 0.4) : undefined,
      "&:hover": {
        textDecorationColor: "inherit",
      },
    }),
    // Same reset as ButtonBase.root
    ...(ownerState.component === "button" && {
      position: "relative",
      WebkitTapHighlightColor: "transparent",
      backgroundColor: "transparent",
      // Reset default value
      // We disable the focus ring for mouse, touch and keyboard users.
      outline: 0,
      border: 0,
      margin: 0,
      // Remove the margin in Safari
      borderRadius: 0,
      padding: 0,
      // Remove the padding in Firefox
      cursor: "pointer",
      userSelect: "none",
      verticalAlign: "middle",
      MozAppearance: "none",
      // Reset
      WebkitAppearance: "none",
      // Reset
      "&::-moz-focus-inner": {
        borderStyle: "none", // Remove Firefox dotted outline.
      },

      [`&.${linkClasses.focusVisible}`]: {
        outline: "auto",
      },
    }),
  };
});

/**
 *
 * Demos:
 *
 * - [Breadcrumbs](https://mui.com/components/breadcrumbs/)
 * - [Links](https://mui.com/components/links/)
 *
 * API:
 *
 * - [Link API](https://mui.com/api/link/)
 * - inherits [Typography API](https://mui.com/api/typography/)
 */
const Link = $$z.component(function Link({ allProps, otherProps, props }) {
  const visible = useIsFocusVisible();
  const [focusVisible, setFocusVisible] = createSignal(false);
  const ownerState = mergeProps(allProps, {
    get focusVisible() {
      return focusVisible();
    },
  });
  const classes = $$z.useClasses(ownerState);
  return createComponent(
    LinkRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        get classes() {
          return props.TypographyClasses;
        },
        get color() {
          return props.color;
        },
        onBlur: (event) => {
          visible.onBlur(event);
          if (visible.isFocusVisibleRef.current === false) {
            setFocusVisible(false);
          }
          if (typeof otherProps.onBlur === "function") {
            otherProps.onBlur(event);
          }
        },
        onFocus: (event) => {
          visible.onFocus(event);
          if (visible.isFocusVisibleRef.current === true) {
            setFocusVisible(true);
          }
          if (typeof otherProps.onFocus === "function") {
            otherProps.onFocus(event);
          }
        },
        ownerState: allProps,
        get variant() {
          return props.variant;
        },
      },
      otherProps,
      {
        get component() {
          return otherProps.component;
        },
        get children() {
          return props.children;
        },
      },
    ),
  );
});

const ListContext = createContext({
  dense: false,
});
function useListContext() {
  return useContext(ListContext);
}

function getListUtilityClass(slot) {
  return generateUtilityClass("MuiList", slot);
}
generateUtilityClasses("MuiList", ["root", "padding", "dense", "subheader"]);

const $$y = createComponentFactory()({
  name: "MuiList",
  selfPropNames: [
    "children",
    "classes",
    "dense",
    "disablePadding",
    "subheader",
  ],
  propDefaults: ({ set }) =>
    set({
      component: "ul",
      dense: false,
      disablePadding: false,
    }),
  utilityClass: getListUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !ownerState.disablePadding && "padding",
      ownerState.dense && "dense",
      !!ownerState.subheader && "subheader",
    ],
  }),
});
const ListRoot = styled$1("ul", {
  name: "MuiList",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      !ownerState.disablePadding && styles.padding,
      ownerState.dense && styles.dense,
      ownerState.subheader && styles.subheader,
    ];
  },
})(({ ownerState }) => ({
  listStyle: "none",
  margin: 0,
  padding: 0,
  position: "relative",
  ...(!ownerState.disablePadding && {
    paddingTop: 8,
    paddingBottom: 8,
  }),
  ...(ownerState.subheader && {
    paddingTop: 0,
  }),
}));

/**
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 * - [Transfer List](https://mui.com/components/transfer-list/)
 *
 * API:
 *
 * - [List API](https://mui.com/api/list/)
 */
const List = $$y.component(function List({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(ListContext.Provider, {
    value: {
      get dense() {
        return props.dense;
      },
    },
    get children() {
      return createComponent(
        ListRoot,
        mergeProps(otherProps, {
          get ["class"]() {
            return clsx(classes.root, otherProps.class);
          },
          ownerState: allProps,
          get children() {
            return [
              createMemo(() => props.subheader),
              createMemo(() => props.children),
            ];
          },
        }),
      );
    },
  });
});

function getListItemButtonUtilityClass(slot) {
  return generateUtilityClass("MuiListItemButton", slot);
}
const listItemButtonClasses = generateUtilityClasses("MuiListItemButton", [
  "root",
  "focusVisible",
  "dense",
  "alignItemsFlexStart",
  "disabled",
  "divider",
  "gutters",
  "selected",
]);

const $$x = createComponentFactory()({
  name: "MuiListItemButton",
  selfPropNames: [
    "alignItems",
    "autoFocus",
    "children",
    "classes",
    "dense",
    "disableGutters",
    "disabled",
    "divider",
    "selected",
  ],
  propDefaults: ({ set }) =>
    set({
      alignItems: "center",
      autoFocus: false,
      component: "div",
      dense: false,
      disableGutters: false,
      divider: false,
      selected: false,
      disabled: false,
    }),
  utilityClass: getListItemButtonUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.dense && "dense",
      !ownerState.disableGutters && "gutters",
      ownerState.divider && "divider",
      ownerState.disabled && "disabled",
      ownerState.alignItems === "flex-start" && "alignItemsFlexStart",
      ownerState.selected && "selected",
    ],
  }),
});
const ListItemButtonRoot = styled$1(ButtonBase, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiListItemButton",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [
      styles.root,
      props.ownerState.dense && styles.dense,
      props.ownerState.alignItems === "flex-start" &&
        styles.alignItemsFlexStart,
      props.ownerState.divider && styles.divider,
      !props.ownerState.disableGutters && styles.gutters,
    ];
  },
})(({ theme, ownerState }) => ({
  display: "flex",
  flexGrow: 1,
  justifyContent: "flex-start",
  alignItems: "center",
  position: "relative",
  textDecoration: "none",
  boxSizing: "border-box",
  textAlign: "left",
  paddingTop: 8,
  paddingBottom: 8,
  transition: theme.transitions.create("background-color", {
    duration: theme.transitions.duration.shortest,
  }),
  "&:hover": {
    textDecoration: "none",
    backgroundColor: theme.palette.action.hover,
    // Reset on touch devices, it doesn't add specificity
    "@media (hover: none)": {
      backgroundColor: "transparent",
    },
  },
  [`&.${listItemButtonClasses.selected}`]: {
    backgroundColor: alpha(
      theme.palette.primary.main,
      theme.palette.action.selectedOpacity,
    ),
    [`&.${listItemButtonClasses.focusVisible}`]: {
      backgroundColor: alpha(
        theme.palette.primary.main,
        theme.palette.action.selectedOpacity +
          theme.palette.action.focusOpacity,
      ),
    },
  },
  [`&.${listItemButtonClasses.selected}:hover`]: {
    backgroundColor: alpha(
      theme.palette.primary.main,
      theme.palette.action.selectedOpacity + theme.palette.action.hoverOpacity,
    ),
    // Reset on touch devices, it doesn't add specificity
    "@media (hover: none)": {
      backgroundColor: alpha(
        theme.palette.primary.main,
        theme.palette.action.selectedOpacity,
      ),
    },
  },
  [`&.${listItemButtonClasses.focusVisible}`]: {
    backgroundColor: theme.palette.action.focus,
  },
  [`&.${listItemButtonClasses.disabled}`]: {
    opacity: theme.palette.action.disabledOpacity,
  },
  ...(ownerState.divider && {
    borderBottom: `1px solid ${theme.palette.divider}`,
    backgroundClip: "padding-box",
  }),
  ...(ownerState.alignItems === "flex-start" && {
    alignItems: "flex-start",
  }),
  ...(!ownerState.disableGutters && {
    paddingLeft: 16,
    paddingRight: 16,
  }),
  ...(ownerState.dense && {
    paddingTop: 4,
    paddingBottom: 4,
  }),
}));

/**
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 *
 * API:
 *
 * - [ListItemButton API](https://mui.com/api/list-item-button/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */

const ListItemButton = $$x.defineComponent(function ListItemButton(inProps) {
  const theme = useTheme$1();
  const props = mergeProps(
    {
      alignItems: "center",
      component: "div",
    },
    theme.components?.[$$x.name]?.defaultProps,
    inProps,
  );
  const [, other] = splitProps(props, [
    "alignItems",
    "autoFocus",
    "component",
    "children",
    "dense",
    "disableGutters",
    "divider",
    "focusVisibleClassName",
    "selected",
  ]);
  const context = useListContext();
  const childContext = {
    get dense() {
      return props.dense || context.dense || false;
    },
    get alignItems() {
      return props.alignItems;
    },
    get disableGutters() {
      return props.disableGutters;
    },
  };
  const element = createElementRef(inProps);
  createEffect(() => {
    if (props.autoFocus) {
      if (element.ref) {
        element.ref.focus();
      }
    }
  });
  const ownerState = {
    get classes() {
      return props.classes;
    },
    get disabled() {
      return props.disabled || false;
    },
    get alignItems() {
      return props.alignItems;
    },
    get dense() {
      return childContext.dense;
    },
    get disableGutters() {
      return props.disableGutters || false;
    },
    get divider() {
      return props.divider || false;
    },
    get selected() {
      return props.selected || false;
    },
  };
  const classes = $$x.useClasses(ownerState);
  return createComponent(ListContext.Provider, {
    value: childContext,
    get children() {
      return createComponent(
        ListItemButtonRoot,
        mergeProps(
          {
            ref: element,
            get component() {
              return props.component;
            },
            get focusVisibleClassName() {
              return clsx(
                props.classes?.focusVisible,
                props.focusVisibleClassName,
              );
            },
            ownerState: ownerState,
          },
          other,
          {
            classes: classes,
            get children() {
              return inProps.children;
            },
          },
        ),
      );
    },
  });
});

function getListItemSecondaryActionClassesUtilityClass(slot) {
  return generateUtilityClass("MuiListItemSecondaryAction", slot);
}
generateUtilityClasses("MuiListItemSecondaryAction", [
  "root",
  "disableGutters",
]);

const $$w = createComponentFactory()({
  name: "MuiListItemSecondaryAction",
  selfPropNames: ["alignItems", "children", "classes"],
  autoCallUseClasses: false,
  utilityClass: getListItemSecondaryActionClassesUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", ownerState.disableGutters && "disableGutters"],
  }),
});
const ListItemSecondaryActionRoot = styled$1("div", {
  name: "MuiListItemSecondaryAction",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.root, ownerState.disableGutters && styles.disableGutters];
  },
})(({ ownerState }) => ({
  position: "absolute",
  right: 16,
  top: "50%",
  transform: "translateY(-50%)",
  ...(ownerState.disableGutters && {
    right: 0,
  }),
}));

/**
 * Must be used as the last child of ListItem to function properly.
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 *
 * API:
 *
 * - [ListItemSecondaryAction API](https://mui.com/api/list-item-secondary-action/)
 */
const ListItemSecondaryAction = $$w.component(function ListItemSecondaryAction({
  allProps,
  otherProps,
  props,
}) {
  const context = useListContext();
  const ownerState = mergeProps(
    {
      get disableGutters() {
        return !!context.disableGutters;
      },
    },
    allProps,
  );
  const classes = $$w.useClasses(ownerState);
  return createComponent(
    ListItemSecondaryActionRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: ownerState,
      get children() {
        return props.children;
      },
    }),
  );
});

function getListItemUtilityClass(slot) {
  return generateUtilityClass("MuiListItem", slot);
}
const listItemClasses = generateUtilityClasses("MuiListItem", [
  "root",
  "container",
  "focusVisible",
  "dense",
  "alignItemsFlexStart",
  "disabled",
  "divider",
  "gutters",
  "padding",
  "button",
  "secondaryAction",
  "selected",
]);

const $$v = createComponentFactory()({
  name: "MuiListItem",
  selfPropNames: [
    "alignItems",
    "autoFocus",
    "children",
    "classes",
    "components",
    "componentsProps",
    "dense",
    "disableGutters",
    "disablePadding",
    "divider",
    "secondaryAction",
  ],
  utilityClass: getListItemUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.dense && "dense",
      !ownerState.disableGutters && "gutters",
      !ownerState.disablePadding && "padding",
      ownerState.divider && "divider",
      ownerState.alignItems === "flex-start" && "alignItemsFlexStart",
    ],
    container: ["container"],
  }),
});
const ListItemRoot = styled$1("div", {
  name: "MuiListItem",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.dense && styles.dense,
      ownerState.alignItems === "flex-start" && styles.alignItemsFlexStart,
      ownerState.divider && styles.divider,
      !ownerState.disableGutters && styles.gutters,
      !ownerState.disablePadding && styles.padding,
      ownerState.button && styles.button,
    ];
  },
})(({ theme, ownerState }) => ({
  display: "flex",
  justifyContent: "flex-start",
  alignItems: "center",
  position: "relative",
  textDecoration: "none",
  width: "100%",
  boxSizing: "border-box",
  textAlign: "left",
  ...(!ownerState.disablePadding && {
    paddingTop: 8,
    paddingBottom: 8,
    ...(ownerState.dense && {
      paddingTop: 4,
      paddingBottom: 4,
    }),
    ...(!ownerState.disableGutters && {
      paddingLeft: 16,
      paddingRight: 16,
    }),
    ...(!!ownerState.secondaryAction && {
      // Add some space to avoid collision as `ListItemSecondaryAction`
      // is absolutely positioned.
      paddingRight: 48,
    }),
  }),
  ...(!!ownerState.secondaryAction && {
    [`& > .${listItemButtonClasses.root}`]: {
      paddingRight: 48,
    },
  }),
  [`&.${listItemClasses.focusVisible}`]: {
    backgroundColor: theme.palette.action.focus,
  },
  [`&.${listItemClasses.disabled}`]: {
    opacity: theme.palette.action.disabledOpacity,
  },
  ...(ownerState.alignItems === "flex-start" && {
    alignItems: "flex-start",
  }),
  ...(ownerState.divider && {
    borderBottom: `1px solid ${theme.palette.divider}`,
    backgroundClip: "padding-box",
  }),
}));

/**
 * Uses an additional container component if `ListItemSecondaryAction` is the last child.
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 * - [Transfer List](https://mui.com/components/transfer-list/)
 *
 * API:
 *
 * - [ListItem API](https://mui.com/api/list-item/)
 */
const ListItem = $$v.defineComponent(function ListItem(inProps) {
  const theme = useTheme$1();
  const baseProps = mergeProps(
    {
      alignItems: "center",
      components: {},
      componentsProps: {},
    },
    theme.components?.[$$v.name]?.defaultProps,
    inProps,
  );
  const context = useContext(ListContext);
  const childContext = {
    get dense() {
      return baseProps.dense || context.dense || false;
    },
    get alignItems() {
      return baseProps.alignItems;
    },
    get disableGutters() {
      return baseProps.disableGutters;
    },
  };
  const element = createElementRef(inProps);
  createEffect(() => {
    if (inProps.autoFocus) {
      if (element.ref) {
        element.ref.focus();
      }
    }
  });

  // [non-reactive root]
  const rootProps = baseProps.componentsProps.root;
  // [non-reactive root]
  const Root = baseProps.components.Root || ListItemRoot;
  const ownerState = mergeProps(
    {
      get classes() {
        return baseProps.classes;
      },
      get alignItems() {
        return baseProps.alignItems;
      },
      get dense() {
        return childContext.dense;
      },
      get disableGutters() {
        return baseProps.disableGutters || false;
      },
      get disablePadding() {
        return baseProps.disablePadding || false;
      },
      get divider() {
        return baseProps.divider || false;
      },
    },
    isHostComponent(Root) ? undefined : rootProps?.ownerState,
  );
  const classes = $$v.useClasses(ownerState);
  const [, other] = splitProps(inProps, [
    "alignItems",
    "autoFocus",
    "children",
    "class",
    "component",
    "components",
    "componentsProps",
    "dense",
    "disableGutters",
    "disablePadding",
    "divider",
    "secondaryAction",
  ]);
  return createComponent(ListContext.Provider, {
    value: childContext,
    get children() {
      return createComponent(
        ListItemRoot,
        mergeProps(
          {
            get as() {
              return baseProps.component || "li";
            },
          },
          rootProps,
          {
            ref: element,
            ownerState: ownerState,
            get ["class"]() {
              return clsx(classes.root, rootProps?.class, inProps.class);
            },
          },
          other,
          {
            get children() {
              return [
                createMemo(() => inProps.children),
                createComponent(Show, {
                  get when() {
                    return inProps.secondaryAction;
                  },
                  children: (secondaryAction) =>
                    createComponent(ListItemSecondaryAction, {
                      get children() {
                        return secondaryAction();
                      },
                    }),
                }),
              ];
            },
          },
        ),
      );
    },
  });
});

function getListItemAvatarUtilityClass(slot) {
  return generateUtilityClass("MuiListItemAvatar", slot);
}
generateUtilityClasses("MuiListItemAvatar", ["root", "alignItemsFlexStart"]);

const $$u = createComponentFactory()({
  name: "MuiListItemAvatar",
  selfPropNames: ["alignItems", "children", "classes"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
    }),
  utilityClass: getListItemAvatarUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.alignItems === "flex-start" && "alignItemsFlexStart",
    ],
  }),
});
const ListItemAvatarRoot = styled$1("div", {
  name: "MuiListItemAvatar",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.alignItems === "flex-start" && styles.alignItemsFlexStart,
    ];
  },
})(({ ownerState }) => ({
  minWidth: 56,
  flexShrink: 0,
  ...(ownerState.alignItems === "flex-start" && {
    marginTop: 8,
  }),
}));

/**
 * A simple wrapper to apply `List` styles to an `Avatar`.
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 *
 * API:
 *
 * - [ListItemAvatar API](https://mui.com/api/list-item-avatar/)
 */
const ListItemAvatar = $$u.component(function ListItemAvatar({
  allProps,
  classes,
  otherProps,
}) {
  const context = useListContext();
  const ownerState = mergeProps(
    {
      get alignItems() {
        return context.alignItems;
      },
    },
    allProps,
  );
  return createComponent(
    ListItemAvatarRoot,
    mergeProps(otherProps, {
      ownerState: ownerState,
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      get children() {
        return allProps.children;
      },
    }),
  );
});

function getListItemIconUtilityClass(slot) {
  return generateUtilityClass("MuiListItemIcon", slot);
}
const listItemIconClasses = generateUtilityClasses("MuiListItemIcon", [
  "root",
  "alignItemsFlexStart",
]);

const $$t = createComponentFactory()({
  name: "MuiListItemIcon",
  selfPropNames: ["alignItems", "children", "classes"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
    }),
  utilityClass: getListItemIconUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.alignItems === "flex-start" && "alignItemsFlexStart",
    ],
  }),
});
const ListItemIconRoot = styled$1("div", {
  name: "MuiListItemIcon",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.alignItems === "flex-start" && styles.alignItemsFlexStart,
    ];
  },
})(({ theme, ownerState }) => ({
  minWidth: 56,
  color: theme.palette.action.active,
  flexShrink: 0,
  display: "inline-flex",
  ...(ownerState.alignItems === "flex-start" && {
    marginTop: 8,
  }),
}));

/**
 * A simple wrapper to apply `List` styles to an `Icon` or `SvgIcon`.
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 *
 * API:
 *
 * - [ListItemIcon API](https://mui.com/api/list-item-icon/)
 */
const ListItemIcon = $$t.component(function ListItemIcon({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const context = useListContext();
  const ownerState = mergeProps(
    {
      get alignItems() {
        return context.alignItems;
      },
    },
    allProps,
  );
  return createComponent(
    ListItemIconRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: ownerState,
      get children() {
        return props.children;
      },
    }),
  );
});

function getListItemTextUtilityClass(slot) {
  return generateUtilityClass("MuiListItemText", slot);
}
const listItemTextClasses = generateUtilityClasses("MuiListItemText", [
  "root",
  "multiline",
  "dense",
  "inset",
  "primary",
  "secondary",
]);

const $$s = createComponentFactory()({
  name: "MuiListItemText",
  selfPropNames: [
    "children",
    "classes",
    "disableTypography",
    "inset",
    "primary",
    "primaryTypographyProps",
    "secondary",
    "secondaryTypographyProps",
  ],
  utilityClass: getListItemTextUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.inset && "inset",
      ownerState.dense && "dense",
      !!ownerState.primary && !!ownerState.secondary && "multiline",
    ],
    primary: ["primary"],
    secondary: ["secondary"],
  }),
});
const ListItemTextRoot = styled$1("div", {
  name: "MuiListItemText",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      {
        [`& .${listItemTextClasses.primary}`]: styles.primary,
      },
      {
        [`& .${listItemTextClasses.secondary}`]: styles.secondary,
      },
      styles.root,
      ownerState.inset && styles.inset,
      ownerState.primary && ownerState.secondary && styles.multiline,
      ownerState.dense && styles.dense,
    ];
  },
})(({ ownerState }) => ({
  flex: "1 1 auto",
  minWidth: 0,
  marginTop: 4,
  marginBottom: 4,
  ...(ownerState.primary &&
    ownerState.secondary && {
      marginTop: 6,
      marginBottom: 6,
    }),
  ...(ownerState.inset && {
    paddingLeft: 56,
  }),
}));

/**
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 *
 * API:
 *
 * - [ListItemText API](https://mui.com/api/list-item-text/)
 */

const ListItemText = $$s.defineComponent(function ListItemText(inProps) {
  const theme = useTheme$1();
  const context = useListContext();
  const props = mergeProps(theme.components?.[$$s.name]?.defaultProps, inProps);
  const primary = createMemo(() => props.primary);
  const secondary = createMemo(() => props.secondary);
  const ownerState = {
    get classes() {
      return props.classes;
    },
    get inset() {
      return props.inset || false;
    },
    get primary() {
      return !!primary();
    },
    get secondary() {
      return !!secondary();
    },
    get dense() {
      return context.dense;
    },
  };
  const classes = $$s.useClasses(ownerState);
  const [, otherProps] = splitProps(props, [
    "children",
    "classes",
    "disableTypography",
    "inset",
    "primary",
    "primaryTypographyProps",
    "secondary",
    "secondaryTypographyProps",
  ]);
  const isDefined = (v) => v !== undefined && v !== null;
  const Primary = () => {
    const $primary = children(() => primary() ?? props.children);
    // [non-reactive root]
    const primaryTypographyProps = props.primaryTypographyProps;
    return createComponent(Show, {
      get when() {
        return (
          isDefined($primary()) &&
          !isSuidElement($primary(), Typography) &&
          !props.disableTypography
        );
      },
      get fallback() {
        return $primary();
      },
      get children() {
        return createComponent(
          Typography,
          mergeProps(
            {
              get variant() {
                return context.dense ? "body2" : "body1";
              },
              get ["class"]() {
                return classes.primary;
              },
              component: "span",
              display: "block",
            },
            primaryTypographyProps,
            {
              get children() {
                return $primary();
              },
            },
          ),
        );
      },
    });
  };
  const Secondary = () => {
    // [non-reactive root]
    const secondaryTypographyProps = props.secondaryTypographyProps;
    return createComponent(Show, {
      get when() {
        return (
          isDefined(secondary()) &&
          !isSuidElement(secondary(), Typography) &&
          !props.disableTypography
        );
      },
      get fallback() {
        return secondary();
      },
      get children() {
        return createComponent(
          Typography,
          mergeProps(
            {
              variant: "body2",
              get ["class"]() {
                return classes.secondary;
              },
              sx: {
                display: "block",
                color: "text.secondary",
              },
            },
            secondaryTypographyProps,
            {
              get component() {
                return secondaryTypographyProps?.component ?? "p";
              },
              get children() {
                return secondary();
              },
            },
          ),
        );
      },
    });
  };
  return createComponent(
    ListItemTextRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: ownerState,
      get children() {
        return [createComponent(Primary, {}), createComponent(Secondary, {})];
      },
    }),
  );
});

function getListSubheaderUtilityClass(slot) {
  return generateUtilityClass("MuiListSubheader", slot);
}
generateUtilityClasses("MuiListSubheader", [
  "root",
  "colorPrimary",
  "colorInherit",
  "gutters",
  "inset",
  "sticky",
]);

const $$r = createComponentFactory()({
  name: "MuiListSubheader",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disableGutters",
    "disableSticky",
    "inset",
  ],
  propDefaults: ({ set }) =>
    set({
      color: "default",
      component: "li",
      disableGutters: false,
      disableSticky: false,
      inset: false,
    }),
  utilityClass: getListSubheaderUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.color !== "default" && `color${capitalize(ownerState.color)}`,
      !ownerState.disableGutters && "gutters",
      ownerState.inset && "inset",
      !ownerState.disableSticky && "sticky",
    ],
  }),
});
const ListSubheaderRoot = styled$1("li", {
  name: "MuiListSubheader",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.color !== "default" &&
        styles[`color${capitalize(ownerState.color)}`],
      !ownerState.disableGutters && styles.gutters,
      ownerState.inset && styles.inset,
      !ownerState.disableSticky && styles.sticky,
    ];
  },
})(({ theme, ownerState }) => ({
  boxSizing: "border-box",
  lineHeight: "48px",
  listStyle: "none",
  color: theme.palette.text.secondary,
  fontFamily: theme.typography.fontFamily,
  fontWeight: theme.typography.fontWeightMedium,
  fontSize: theme.typography.pxToRem(14),
  ...(ownerState.color === "primary" && {
    color: theme.palette.primary.main,
  }),
  ...(ownerState.color === "inherit" && {
    color: "inherit",
  }),
  ...(!ownerState.disableGutters && {
    paddingLeft: 16,
    paddingRight: 16,
  }),
  ...(ownerState.inset && {
    paddingLeft: 72,
  }),
  ...(!ownerState.disableSticky && {
    position: "sticky",
    top: 0,
    zIndex: 1,
    backgroundColor: theme.palette.background.paper,
  }),
}));

/**
 *
 * Demos:
 *
 * - [Lists](https://mui.com/components/lists/)
 *
 * API:
 *
 * - [ListSubheader API](https://mui.com/api/list-subheader/)
 */

const ListSubheader = $$r.component(function ListSubheader({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(
    ListSubheaderRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: allProps,
      get children() {
        return props.children;
      },
    }),
  );
});

const $$q = createComponentFactory()({
  name: "MuiMenuList",
  selfPropNames: [
    "autoFocus",
    "autoFocusItem",
    "children",
    "disabledItemsFocusable",
    "disableListWrap",
    "variant",
  ],
});
function nextItem(list, item, disableListWrap) {
  if (list === item) {
    return list.firstChild;
  }
  if (item && item.nextElementSibling) {
    return item.nextElementSibling;
  }
  return disableListWrap ? undefined : list.firstChild;
}
function previousItem(list, item, disableListWrap) {
  if (list === item) {
    return disableListWrap ? list.firstChild : list.lastChild;
  }
  if (item && item.previousElementSibling) {
    return item.previousElementSibling;
  }
  return disableListWrap ? undefined : list.lastChild;
}
function textCriteriaMatches(nextFocus, textCriteria) {
  if (textCriteria === undefined) {
    return true;
  }
  let text = nextFocus.innerText;
  if (text === undefined) {
    // jsdom doesn't support innerText
    text = nextFocus.textContent ?? "";
  }
  text = text.trim().toLowerCase();
  if (text.length === 0) {
    return false;
  }
  if (textCriteria.repeating) {
    return text[0] === textCriteria.keys[0];
  }
  return text.indexOf(textCriteria.keys.join("")) === 0;
}
function moveFocus(
  list,
  currentFocus,
  disableListWrap,
  disabledItemsFocusable,
  traversalFunction,
  textCriteria,
) {
  let wrappedOnce = false;
  let nextFocus = traversalFunction(
    list,
    currentFocus,
    currentFocus ? disableListWrap : false,
  );
  while (nextFocus) {
    // Prevent infinite loop.
    if (nextFocus === list.firstChild) {
      if (wrappedOnce) {
        return false;
      }
      wrappedOnce = true;
    }

    // Same logic as useAutocomplete.js
    const nextFocusDisabled = disabledItemsFocusable
      ? false
      : nextFocus["disabled"] ||
        nextFocus.getAttribute("aria-disabled") === "true";
    if (
      !nextFocus.hasAttribute("tabindex") ||
      !textCriteriaMatches(nextFocus, textCriteria) ||
      nextFocusDisabled
    ) {
      // Move to the next element.
      nextFocus = traversalFunction(list, nextFocus, disableListWrap);
    } else {
      nextFocus.focus();
      return true;
    }
  }
  return false;
}

/**
 * A permanently displayed menu following https://www.w3.org/TR/wai-aria-practices/#menubutton.
 * It's exposed to help customization of the [`Menu`](/api/menu/) component if you
 * use it separately you need to move focus into the component manually. Once
 * the focus is placed inside the component it is fully keyboard accessible.
 */
/**
 * A permanently displayed menu following https://www.w3.org/TR/wai-aria-practices/#menubutton.
 * It's exposed to help customization of the [`Menu`](https://mui.com/api/menu/) component if you
 * use it separately you need to move focus into the component manually. Once
 * the focus is placed inside the component it is fully keyboard accessible.
 *
 * Demos:
 *
 * - [Menus](https://mui.com/components/menus/)
 *
 * API:
 *
 * - [MenuList API](https://mui.com/api/menu-list/)
 * - inherits [List API](https://mui.com/api/list/)
 */
const MenuList = $$q.defineComponent(function MenuList(props) {
  const listRef = createRef(props);
  const [, other] = splitProps(props, [
    "autoFocus",
    "autoFocusItem",
    "children",
    "class",
    "disabledItemsFocusable",
    "disableListWrap",
    "onKeyDown",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      autoFocus: false,
      autoFocusItem: false,
      disabledItemsFocusable: false,
      disableListWrap: false,
      variant: "selectedMenu",
    },
    props,
  );
  const criteria = {
    keys: [],
    repeating: true,
    previousKeyMatched: true,
    lastTime: 0,
  };
  createEffect(() => {
    if (baseProps.autoFocus) {
      listRef.current.focus();
    }
  }, [baseProps.autoFocus]);
  const handleKeyDown = (event) => {
    const list = listRef.current;
    const key = event.key;
    /**
     * @type {Element} - will always be defined since we are in a keydown handler
     * attached to an element. A keydown event is either dispatched to the activeElement
     * or document.body or document.documentElement. Only the first case will
     * trigger this specific handler.
     */
    const currentFocus = ownerDocument(list).activeElement;
    if (key === "ArrowDown") {
      // Prevent scroll of the page
      event.preventDefault();
      moveFocus(
        list,
        currentFocus,
        baseProps.disableListWrap,
        baseProps.disabledItemsFocusable,
        nextItem,
      );
    } else if (key === "ArrowUp") {
      event.preventDefault();
      moveFocus(
        list,
        currentFocus,
        baseProps.disableListWrap,
        baseProps.disabledItemsFocusable,
        previousItem,
      );
    } else if (key === "Home") {
      event.preventDefault();
      moveFocus(
        list,
        undefined,
        baseProps.disableListWrap,
        baseProps.disabledItemsFocusable,
        nextItem,
      );
    } else if (key === "End") {
      event.preventDefault();
      moveFocus(
        list,
        undefined,
        baseProps.disableListWrap,
        baseProps.disabledItemsFocusable,
        previousItem,
      );
    } else if (key.length === 1) {
      const lowerKey = key.toLowerCase();
      const currTime = performance.now();
      if (criteria.keys.length > 0) {
        // Reset
        if (currTime - criteria.lastTime > 500) {
          criteria.keys = [];
          criteria.repeating = true;
          criteria.previousKeyMatched = true;
        } else if (criteria.repeating && lowerKey !== criteria.keys[0]) {
          criteria.repeating = false;
        }
      }
      criteria.lastTime = currTime;
      criteria.keys.push(lowerKey);
      const keepFocusOnCurrent =
        currentFocus &&
        !criteria.repeating &&
        textCriteriaMatches(currentFocus, criteria);
      if (
        criteria.previousKeyMatched &&
        (keepFocusOnCurrent ||
          moveFocus(
            list,
            currentFocus,
            false,
            baseProps.disabledItemsFocusable,
            nextItem,
            criteria,
          ))
      ) {
        event.preventDefault();
      } else {
        criteria.previousKeyMatched = false;
      }
    }
    if (typeof props.onKeyDown === "function") {
      props.onKeyDown(event);
    }
  };
  /**
   * the index of the item should receive focus
   * in a `variant="selectedMenu"` it's the first `selected` item
   * otherwise it's the very first item.
   */

  const children = inspectChildren(() => props.children);

  /**
   * the index of the item should receive focus
   * in a `variant="selectedMenu"` it's the first `selected` item
   * otherwise it's the very first item.
   */
  const activeItemIndex = createMemo(() => {
    let result = -1;
    let index = 0;
    // since we inject focus related props into children we have to do a lookahead
    // to check if there is a `selected` item. We're looking for the last `selected`
    // item and use the first valid item as a fallback
    for (const child of children()) {
      if (isComponentObject(child)) {
        if (!child.props.disabled) {
          if (baseProps.variant === "selectedMenu" && child.props.selected) {
            result = index;
          } else if (result === -1) {
            result = index;
          }
        }
      }
      index++;
    }
    return result;
  });
  const items = mapArray(children, (child, index) => {
    if (isComponentObject(child)) {
      return createComponent(
        child.Component,
        mergeProps(child.props, {
          get autoFocus() {
            if (index() === activeItemIndex() && baseProps.autoFocusItem)
              return true;
          },
          get tabIndex() {
            if (
              child.props.tabIndex === undefined &&
              index() === activeItemIndex() &&
              baseProps.variant
            )
              return 0;
          },
        }),
      );
    } else {
      return child;
    }
  });
  return createComponent(
    List,
    mergeProps(
      {
        role: "menu",
        ref: listRef,
        get ["class"]() {
          return props.class;
        },
        onKeyDown: handleKeyDown,
        get tabIndex() {
          return baseProps.autoFocus ? 0 : -1;
        },
      },
      other,
      {
        get children() {
          return items();
        },
      },
    ),
  );
});

function getPopoverUtilityClass(slot) {
  return generateUtilityClass("MuiPopover", slot);
}
generateUtilityClasses("MuiPopover", ["root", "paper"]);

const $$p = createComponentFactory()({
  name: "MuiPopover",
  selfPropNames: [
    "action",
    "anchorEl",
    "anchorOrigin",
    "anchorPosition",
    "anchorReference",
    "children",
    "classes",
    "container",
    "elevation",
    "marginThreshold",
    "onClose",
    "open",
    "PaperProps",
    "transformOrigin",
    "TransitionComponent",
    "transitionDuration",
    "TransitionProps",
  ],
  utilityClass: getPopoverUtilityClass,
  slotClasses: () => ({
    root: ["root"],
    paper: ["paper"],
  }),
});
function getOffsetTop(rect, vertical) {
  let offset = 0;
  if (typeof vertical === "number") {
    offset = vertical;
  } else if (vertical === "center") {
    offset = rect.height / 2;
  } else if (vertical === "bottom") {
    offset = rect.height;
  }
  return offset;
}
function getOffsetLeft(rect, horizontal) {
  let offset = 0;
  if (typeof horizontal === "number") {
    offset = horizontal;
  } else if (horizontal === "center") {
    offset = rect.width / 2;
  } else if (horizontal === "right") {
    offset = rect.width;
  }
  return offset;
}
function getTransformOriginValue(transformOrigin) {
  return [transformOrigin.horizontal, transformOrigin.vertical]
    .map((n) => (typeof n === "number" ? `${n}px` : n))
    .join(" ");
}
function resolveAnchorEl$1(anchorEl) {
  return typeof anchorEl === "function" ? anchorEl() : anchorEl;
}
const PopoverRoot = styled$1(Modal, {
  name: "MuiPopover",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({});
const PopoverPaper = styled$1(Paper, {
  name: "MuiPopover",
  slot: "Paper",
  overridesResolver: (props, styles) => styles.paper,
})({
  position: "absolute",
  overflowY: "auto",
  overflowX: "hidden",
  // So we see the popover when it's empty.
  // It's most likely on issue on userland.
  minWidth: 16,
  minHeight: 16,
  maxWidth: "calc(100% - 32px)",
  maxHeight: "calc(100% - 32px)",
  // We disable the focus ring for mouse, touch and keyboard users.
  outline: 0,
});
const Popover = $$p.defineComponent(function Popover(inProps) {
  const props = $$p.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "action",
    "anchorEl",
    "anchorOrigin",
    "anchorPosition",
    "anchorReference",
    "children",
    "class",
    "container",
    "elevation",
    "marginThreshold",
    "open",
    "PaperProps",
    "transformOrigin",
    "TransitionComponent",
    "transitionDuration",
    "TransitionProps",
  ]);
  const baseProps = mergeProps(
    {
      anchorOrigin: {
        vertical: "top",
        horizontal: "left",
      },
      anchorReference: "anchorEl",
      elevation: 8,
      marginThreshold: 16,
      PaperProps: {},
      transformOrigin: {
        vertical: "top",
        horizontal: "left",
      },
      TransitionComponent: Grow,
      transitionDuration: "auto",
    },
    props,
  );
  const [, TransitionProps] = splitProps(
    mergeProps(() => props.TransitionProps || {}),
    ["onEntering"],
  );
  const paperRef = createRef(() => baseProps.PaperProps.ref);
  const ownerState = mergeProps(props, {
    get anchorOrigin() {
      return baseProps.anchorOrigin;
    },
    get anchorReference() {
      return baseProps.anchorReference;
    },
    get elevation() {
      return baseProps.elevation;
    },
    get marginThreshold() {
      return baseProps.marginThreshold;
    },
    get PaperProps() {
      return baseProps.PaperProps;
    },
    get transformOrigin() {
      return baseProps.transformOrigin;
    },
    get TransitionComponent() {
      return baseProps.TransitionComponent;
    },
    get transitionDuration() {
      return baseProps.transitionDuration;
    },
    TransitionProps: TransitionProps,
  });
  const classes = $$p.useClasses(ownerState);

  // Returns the top/left offset of the position
  // to attach to on the anchor element (or body if none is provided)
  const getAnchorOffset = () => {
    if (baseProps.anchorReference === "anchorPosition") {
      return props.anchorPosition;
    }
    const resolvedAnchorEl = resolveAnchorEl$1(props.anchorEl);

    // If an anchor element wasn't provided, just use the parent body element of this Popover
    const anchorElement =
      resolvedAnchorEl && resolvedAnchorEl.nodeType === 1
        ? resolvedAnchorEl
        : ownerDocument(paperRef.current).body;
    const anchorRect = anchorElement.getBoundingClientRect();
    return {
      top:
        anchorRect.top +
        getOffsetTop(anchorRect, baseProps.anchorOrigin.vertical),
      left:
        anchorRect.left +
        getOffsetLeft(anchorRect, baseProps.anchorOrigin.horizontal),
    };
  };

  // Returns the base transform origin using the element
  const getTransformOrigin = (elemRect) => {
    return {
      vertical: getOffsetTop(elemRect, baseProps.transformOrigin.vertical),
      horizontal: getOffsetLeft(elemRect, baseProps.transformOrigin.horizontal),
    };
  };
  const getPositioningStyle = (element) => {
    const elemRect = {
      width: element.offsetWidth,
      height: element.offsetHeight,
    };

    // Get the transform origin point on the element itself
    const elemTransformOrigin = getTransformOrigin(elemRect);
    if (baseProps.anchorReference === "none") {
      return {
        top: null,
        left: null,
        transformOrigin: getTransformOriginValue(elemTransformOrigin),
      };
    }

    // Get the offset of the anchoring element
    const anchorOffset = getAnchorOffset();

    // Calculate element positioning
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    let top = anchorOffset.top - elemTransformOrigin.vertical;
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    let left = anchorOffset.left - elemTransformOrigin.horizontal;
    const bottom = top + elemRect.height;
    const right = left + elemRect.width;

    // Use the parent window of the anchorEl if provided
    const containerWindow = ownerWindow(resolveAnchorEl$1(props.anchorEl));

    // Window thresholds taking required margin into account
    const heightThreshold =
      containerWindow.innerHeight - baseProps.marginThreshold;
    const widthThreshold =
      containerWindow.innerWidth - baseProps.marginThreshold;

    // Check if the vertical axis needs shifting
    if (top < baseProps.marginThreshold) {
      const diff = top - baseProps.marginThreshold;
      top -= diff;
      elemTransformOrigin.vertical += diff;
    } else if (bottom > heightThreshold) {
      const diff = bottom - heightThreshold;
      top -= diff;
      elemTransformOrigin.vertical += diff;
    }

    // Check if the horizontal axis needs shifting
    if (left < baseProps.marginThreshold) {
      const diff = left - baseProps.marginThreshold;
      left -= diff;
      elemTransformOrigin.horizontal += diff;
    } else if (right > widthThreshold) {
      const diff = right - widthThreshold;
      left -= diff;
      elemTransformOrigin.horizontal += diff;
    }
    return {
      top: `${Math.round(top)}px`,
      left: `${Math.round(left)}px`,
      transformOrigin: getTransformOriginValue(elemTransformOrigin),
    };
  };
  const setPositioningStyles = () => {
    const element = paperRef.current;
    if (!element) {
      return;
    }
    const positioning = getPositioningStyle(element);
    if (positioning.top !== null) {
      element.style.top = positioning.top;
    }
    if (positioning.left !== null) {
      element.style.left = positioning.left;
    }
    element.style.transformOrigin = positioning.transformOrigin;
  };
  const handleEntering = () => {
    if (props.TransitionProps?.onEntering) {
      props.TransitionProps?.onEntering();
    }
    setPositioningStyles();
  };
  createEffect(() => {
    if (props.open) {
      setPositioningStyles();
    }
  });
  if (typeof props.action === "function") {
    props.action({
      updatePosition: () => {
        if (props.open) setPositioningStyles();
      },
    });
  }
  let handleResize;
  let handleResizeCleanup;
  let resizeObserver;
  createEffect(
    on(
      () => [props.anchorEl, props.open, setPositioningStyles],
      () => {
        handleResizeCleanup?.();
        if (!props.open) return undefined;
        const anchor = resolveAnchorEl$1(props.anchorEl);
        const containerWindow = ownerWindow(anchor);
        handleResize = debounce$1(() => setPositioningStyles());
        handleResizeCleanup = () => {
          if (handleResize) {
            handleResize.clear();
            containerWindow.removeEventListener("resize", handleResize);
            handleResize = undefined;
          }
          if (resizeObserver) {
            resizeObserver.disconnect();
            resizeObserver = undefined;
          }
        };
        if (anchor && globalThis.ResizeObserver) {
          resizeObserver = new ResizeObserver(() => setPositioningStyles());
          resizeObserver.observe(anchor);
        }
        containerWindow.addEventListener("resize", handleResize);
      },
    ),
  );
  onCleanup(() => handleResizeCleanup?.());
  const transitionDuration = () => {
    let transitionDuration = baseProps.transitionDuration;
    if (
      baseProps.transitionDuration === "auto" &&
      !baseProps.TransitionComponent.muiSupportAuto
    ) {
      transitionDuration = undefined;
    }
    return transitionDuration;
  };

  // If the container prop is provided, use that
  // If the anchorEl prop is provided, use its parent body element as the container
  // If neither are provided let the Modal take care of choosing the container
  const container = () =>
    props.container ||
    (props.anchorEl
      ? ownerDocument(resolveAnchorEl$1(props.anchorEl)).body
      : undefined);
  return createComponent(
    PopoverRoot,
    mergeProps(
      {
        BackdropProps: {
          invisible: true,
        },
        get ["class"]() {
          return clsx(classes.root, props.class);
        },
        get container() {
          return container();
        },
        get open() {
          return props.open;
        },
        ownerState: ownerState,
      },
      other,
      {
        get children() {
          return createComponent(
            baseProps.TransitionComponent,
            mergeProps(
              {
                appear: true,
                get ["in"]() {
                  return props.open;
                },
                onEntering: handleEntering,
                get timeout() {
                  return transitionDuration();
                },
              },
              TransitionProps,
              {
                get children() {
                  return createComponent(
                    PopoverPaper,
                    mergeProps(
                      {
                        get elevation() {
                          return baseProps.elevation;
                        },
                      },
                      () => baseProps.PaperProps,
                      {
                        ref: paperRef,
                        get ["class"]() {
                          return clsx(
                            classes.paper,
                            baseProps.PaperProps.class,
                          );
                        },
                        get children() {
                          return props.children;
                        },
                      },
                    ),
                  );
                },
              },
            ),
          );
        },
      },
    ),
  );
});

function getMenuUtilityClass(slot) {
  return generateUtilityClass("MuiMenu", slot);
}
generateUtilityClasses("MuiMenu", ["root", "paper", "list"]);

const $$o = createComponentFactory()({
  name: "MuiMenu",
  selfPropNames: [
    "anchorEl",
    "autoFocus",
    "children",
    "classes",
    "disableAutoFocusItem",
    "MenuListProps",
    "onClose",
    "open",
    "PopoverClasses",
    "transitionDuration",
    "TransitionProps",
    "variant",
  ],
  utilityClass: getMenuUtilityClass,
  slotClasses: () => ({
    root: ["root"],
    paper: ["paper"],
    list: ["list"],
  }),
});
const RTL_ORIGIN = {
  vertical: "top",
  horizontal: "right",
};
const LTR_ORIGIN = {
  vertical: "top",
  horizontal: "left",
};
const MenuRoot = styled$1(Popover, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiMenu",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({});
const MenuPaper = styled$1(Paper, {
  name: "MuiMenu",
  slot: "Paper",
  overridesResolver: (props, styles) => styles.paper,
})({
  // specZ: The maximum height of a simple menu should be one or more rows less than the view
  // height. This ensures a tapable area outside of the simple menu with which to dismiss
  // the menu.
  maxHeight: "calc(100% - 96px)",
  // Add iOS momentum scrolling for iOS < 13.0
  WebkitOverflowScrolling: "touch",
});
const MenuMenuList = styled$1(MenuList, {
  name: "MuiMenu",
  slot: "List",
  overridesResolver: (props, styles) => styles.list,
})({
  // We disable the focus ring for mouse, touch and keyboard users.
  outline: 0,
});
const Menu = $$o.defineComponent(function Menu(inProps) {
  const ref = createRef(inProps);
  const props = $$o.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "autoFocus",
    "children",
    "disableAutoFocusItem",
    "MenuListProps",
    "onClose",
    "open",
    "PaperProps",
    "PopoverClasses",
    "transitionDuration",
    "TransitionProps",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      autoFocus: true,
      disableAutoFocusItem: false,
      MenuListProps: {},
      PaperProps: {},
      transitionDuration: "auto",
      variant: "selectedMenu",
    },
    props,
  );
  const [, TransitionProps] = splitProps(
    mergeProps(() => props.TransitionProps || {}),
    ["onEntering"],
  );
  const theme = useTheme$1();
  const isRtl = () => theme.direction === "rtl";
  const ownerState = mergeProps(baseProps, {
    TransitionProps: TransitionProps,
  });
  const classes = $$o.useClasses(ownerState);
  const autoFocusItem = () =>
    baseProps.autoFocus && !baseProps.disableAutoFocusItem && props.open;

  /*const menuListActionsRef = null;*/

  const handleEntering = () => {
    /*
    // [pending]
    if (menuListActionsRef.current) {
      menuListActionsRef.current.adjustStyleForScrollbar(element, theme);
    }*/

    if (props.TransitionProps?.onEntering) {
      props.TransitionProps?.onEntering();
    }
  };
  const handleListKeyDown = (event) => {
    if (event.key === "Tab") {
      event.preventDefault();
      if (props.onClose) {
        props.onClose(event, "tabKeyDown");
      }
    }
  };
  /**
   * the index of the item should receive focus
   * in a `variant="selectedMenu"` it's the first `selected` item
   * otherwise it's the very first item.
   */
  const children = inspectChildren(() => props.children);

  /**
   * the index of the item should receive focus
   * in a `variant="selectedMenu"` it's the first `selected` item
   * otherwise it's the very first item.
   */
  const activeItemIndex = () => {
    let result = -1;
    let index = 0;
    // since we inject focus related props into children we have to do a lookahead
    // to check if there is a `selected` item. We're looking for the last `selected`
    // item and use the first valid item as a fallback
    for (const child of children()) {
      if (isComponentObject(child)) {
        if (!child.props.disabled) {
          if (baseProps.variant === "selectedMenu" && child.props.selected) {
            result = index;
          } else if (result === -1) {
            result = index;
          }
        }
      }
      index++;
    }
    return result;
  };
  const PaperProps = mergeProps(
    {
      component: MenuPaper,
    },
    () => baseProps.PaperProps,
    {
      classes: mergeProps(() => baseProps.PaperProps.classes, {
        get root() {
          return classes.paper;
        },
      }),
    },
  );
  const transitionProps = mergeProps(
    {
      onEntering: handleEntering,
    },
    TransitionProps,
  );
  return createComponent(
    MenuRoot,
    mergeProps(
      {
        get classes() {
          return props.PopoverClasses;
        },
        get onClose() {
          return props.onClose;
        },
        anchorOrigin: {
          vertical: "bottom",
          get horizontal() {
            return isRtl() ? "right" : "left";
          },
        },
        get transformOrigin() {
          return isRtl() ? RTL_ORIGIN : LTR_ORIGIN;
        },
        PaperProps: PaperProps,
        get ["class"]() {
          return classes.root;
        },
        get open() {
          return props.open;
        },
        ref: ref,
        get transitionDuration() {
          return baseProps.transitionDuration;
        },
        TransitionProps: transitionProps,
        ownerState: ownerState,
      },
      other,
      {
        get children() {
          return createComponent(
            MenuMenuList,
            mergeProps(
              {
                onKeyDown: handleListKeyDown,
                get autoFocus() {
                  return (
                    createMemo(() => !!baseProps.autoFocus)() &&
                    (activeItemIndex() === -1 || baseProps.disableAutoFocusItem)
                  );
                },
                get autoFocusItem() {
                  return autoFocusItem();
                },
                get variant() {
                  return baseProps.variant;
                },
              },
              () => baseProps.MenuListProps,
              {
                get ["class"]() {
                  return clsx(classes.list, baseProps.MenuListProps.class);
                },
                get children() {
                  return children();
                },
              },
            ),
          );
        },
      },
    ),
  );
});

function getMenuItemUtilityClass(slot) {
  return generateUtilityClass("MuiMenuItem", slot);
}
const menuItemClasses = generateUtilityClasses("MuiMenuItem", [
  "root",
  "focusVisible",
  "dense",
  "disabled",
  "divider",
  "gutters",
  "selected",
]);

const $$n = createComponentFactory()({
  name: "MuiMenuItem",
  selfPropNames: [
    "autoFocus",
    "classes",
    "dense",
    "disabled",
    "disableGutters",
    "divider",
    "selected",
  ],
  utilityClass: getMenuItemUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.dense && "dense",
      ownerState.disabled && "disabled",
      !ownerState.disableGutters && "gutters",
      ownerState.divider && "divider",
      ownerState.selected && "selected",
    ],
  }),
});
const MenuItemRoot = styled$1(ButtonBase, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiMenuItem",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.dense && styles.dense,
      ownerState.divider && styles.divider,
      !ownerState.disableGutters && styles.gutters,
    ];
  },
})(({ theme, ownerState }) => ({
  ...theme.typography.body1,
  display: "flex",
  justifyContent: "flex-start",
  alignItems: "center",
  position: "relative",
  textDecoration: "none",
  minHeight: 48,
  paddingTop: 6,
  paddingBottom: 6,
  boxSizing: "border-box",
  whiteSpace: "nowrap",
  ...(!ownerState.disableGutters && {
    paddingLeft: 16,
    paddingRight: 16,
  }),
  ...(ownerState.divider && {
    borderBottom: `1px solid ${theme.palette.divider}`,
    backgroundClip: "padding-box",
  }),
  "&:hover": {
    textDecoration: "none",
    backgroundColor: theme.palette.action.hover,
    // Reset on touch devices, it doesn't add specificity
    "@media (hover: none)": {
      backgroundColor: "transparent",
    },
  },
  [`&.${menuItemClasses.selected}`]: {
    backgroundColor: alpha(
      theme.palette.primary.main,
      theme.palette.action.selectedOpacity,
    ),
    [`&.${menuItemClasses.focusVisible}`]: {
      backgroundColor: alpha(
        theme.palette.primary.main,
        theme.palette.action.selectedOpacity +
          theme.palette.action.focusOpacity,
      ),
    },
  },
  [`&.${menuItemClasses.selected}:hover`]: {
    backgroundColor: alpha(
      theme.palette.primary.main,
      theme.palette.action.selectedOpacity + theme.palette.action.hoverOpacity,
    ),
    // Reset on touch devices, it doesn't add specificity
    "@media (hover: none)": {
      backgroundColor: alpha(
        theme.palette.primary.main,
        theme.palette.action.selectedOpacity,
      ),
    },
  },
  [`&.${menuItemClasses.focusVisible}`]: {
    backgroundColor: theme.palette.action.focus,
  },
  [`&.${menuItemClasses.disabled}`]: {
    opacity: theme.palette.action.disabledOpacity,
  },
  [`& + .${dividerClasses.root}`]: {
    marginTop: theme.spacing(1),
    marginBottom: theme.spacing(1),
  },
  [`& + .${dividerClasses.inset}`]: {
    marginLeft: 52,
  },
  [`& .${listItemTextClasses.root}`]: {
    marginTop: 0,
    marginBottom: 0,
  },
  [`& .${listItemTextClasses.inset}`]: {
    paddingLeft: 36,
  },
  [`& .${listItemIconClasses.root}`]: {
    minWidth: 36,
  },
  ...(!ownerState.dense && {
    [theme.breakpoints.up("sm")]: {
      minHeight: "auto",
    },
  }),
  ...(ownerState.dense && {
    minHeight: 32,
    // https://material.io/components/menus#specs > Dense
    paddingTop: 4,
    paddingBottom: 4,
    ...theme.typography.body2,
    [`& .${listItemIconClasses.root} svg`]: {
      fontSize: "1.25rem",
    },
  }),
}));

/**
 *
 * Demos:
 *
 * - [Menus](https://mui.com/components/menus/)
 *
 * API:
 *
 * - [MenuItem API](https://mui.com/api/menu-item/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */
const MenuItem = $$n.defineComponent(function MenuItem(inProps) {
  const menuItemRef = createRef(inProps);
  const props = $$n.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "autoFocus",
    "component",
    "dense",
    "divider",
    "disableGutters",
    "focusVisibleClassName",
    "role",
    "tabIndex",
  ]);
  const baseProps = mergeProps(
    {
      autoFocus: false,
      component: "li",
      dense: false,
      divider: false,
      disableGutters: false,
      role: "menuitem",
    },
    props,
  );
  const context = useContext(ListContext);
  const childContext = {
    get dense() {
      return baseProps.dense || context.dense || false;
    },
    get disableGutters() {
      return baseProps.disableGutters;
    },
  };
  createEffect(() => {
    if (baseProps.autoFocus) {
      if (menuItemRef.current) {
        untrack(() => menuItemRef.current.focus());
      }
    }
  });
  const ownerState = mergeProps(props, {
    get dense() {
      return childContext.dense;
    },
    get divider() {
      return baseProps.divider;
    },
    get disableGutters() {
      return baseProps.disableGutters;
    },
  });
  const classes = $$n.useClasses(props);
  const tabIndex = () => {
    if (!props.disabled) {
      return props.tabIndex !== undefined ? props.tabIndex : -1;
    }
  };
  return createComponent(ListContext.Provider, {
    value: childContext,
    get children() {
      return createComponent(
        MenuItemRoot,
        mergeProps(
          {
            ref: menuItemRef,
            get role() {
              return baseProps.role;
            },
            get tabIndex() {
              return tabIndex();
            },
            get component() {
              return baseProps.component;
            },
            get focusVisibleClassName() {
              return clsx(
                ownerState.classes?.focusVisible,
                props.focusVisibleClassName,
              );
            },
          },
          other,
          {
            ownerState: ownerState,
            classes: classes,
          },
        ),
      );
    },
  });
});

const _tmpl$$5 = /*#__PURE__*/ template(
  `<svg><path d="M7 10l5 5 5-5z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const ArrowDropDownIcon = createSvgIcon(() => _tmpl$$5(), "ArrowDropDown");

function getNativeSelectUtilityClasses(slot) {
  return generateUtilityClass("MuiNativeSelect", slot);
}
const nativeSelectClasses = generateUtilityClasses("MuiNativeSelect", [
  "root",
  "select",
  "multiple",
  "filled",
  "outlined",
  "standard",
  "disabled",
  "icon",
  "iconOpen",
  "iconFilled",
  "iconOutlined",
  "iconStandard",
  "nativeInput",
]);

const useUtilityClasses = (ownerState) => {
  const slots = {
    select: [
      "select",
      ownerState.variant,
      ownerState.disabled && "disabled",
      ownerState.multiple && "multiple",
    ],
    icon: [
      "icon",
      ownerState.variant && `icon${capitalize(ownerState.variant)}`,
      ownerState.open && "iconOpen",
      ownerState.disabled && "disabled",
    ],
  };
  return composeClasses(
    slots,
    getNativeSelectUtilityClasses,
    ownerState.classes,
  );
};
const nativeSelectSelectStyles = ({ ownerState, theme }) => ({
  MozAppearance: "none",
  // Reset
  WebkitAppearance: "none",
  // Reset
  // When interacting quickly, the text can end up selected.
  // Native select can't be selected either.
  userSelect: "none",
  borderRadius: 0,
  // Reset
  cursor: "pointer",
  "&:focus": {
    // Show that it's not an text input
    backgroundColor:
      theme.palette.mode === "light"
        ? "rgba(0, 0, 0, 0.05)"
        : "rgba(255, 255, 255, 0.05)",
    borderRadius: 0, // Reset Chrome style
  },

  // Remove IE11 arrow
  "&::-ms-expand": {
    display: "none",
  },
  [`&.${nativeSelectClasses.disabled}`]: {
    cursor: "default",
  },
  "&[multiple]": {
    height: "auto",
  },
  "&:not([multiple]) option, &:not([multiple]) optgroup": {
    backgroundColor: theme.palette.background.paper,
  },
  // Bump specificity to allow extending custom inputs
  "&&&": {
    paddingRight: 24,
    minWidth: 16, // So it doesn't collapse.
  },

  ...(ownerState.variant === "filled" && {
    "&&&": {
      paddingRight: 32,
    },
  }),
  ...(ownerState.variant === "outlined" && {
    borderRadius: theme.shape.borderRadius,
    "&:focus": {
      borderRadius: theme.shape.borderRadius, // Reset the reset for Chrome style
    },

    "&&&": {
      paddingRight: 32,
    },
  }),
});
const NativeSelectSelect = styled$1("select", {
  name: "MuiNativeSelect",
  slot: "Select",
  skipProps: skipRootProps,
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.select,
      styles[ownerState.variant],
      {
        [`&.${nativeSelectClasses.multiple}`]: styles.multiple,
      },
    ];
  },
})(nativeSelectSelectStyles);
const nativeSelectIconStyles = ({ ownerState, theme }) => ({
  // We use a position absolute over a flexbox in order to forward the pointer events
  // to the input and to support wrapping tags..
  position: "absolute",
  right: 0,
  top: "calc(50% - .5em)",
  // Center vertically, height is 1em
  pointerEvents: "none",
  // Don't block pointer events on the select under the icon.
  color: theme.palette.action.active,
  [`&.${nativeSelectClasses.disabled}`]: {
    color: theme.palette.action.disabled,
  },
  ...(ownerState.open && {
    transform: "rotate(180deg)",
  }),
  ...(ownerState.variant === "filled" && {
    right: 7,
  }),
  ...(ownerState.variant === "outlined" && {
    right: 7,
  }),
});
const NativeSelectIcon = styled$1("svg", {
  name: "MuiNativeSelect",
  slot: "Icon",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.icon,
      ownerState.variant && styles[`icon${capitalize(ownerState.variant)}`],
      ownerState.open && styles.iconOpen,
    ];
  },
})(nativeSelectIconStyles);

/**
 * @ignore - internal component.
 */
const NativeSelectInput = function NativeSelectInput(props) {
  const [, other] = splitProps(props, [
    "class",
    "disabled",
    "IconComponent",
    "inputRef",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      variant: "standard",
    },
    props,
  );
  const ownerState = mergeProps(props, {
    get disabled() {
      return props.disabled;
    },
    get variant() {
      return baseProps.variant;
    },
  });
  const classes = useUtilityClasses(ownerState);
  return [
    createComponent(
      NativeSelectSelect,
      mergeProps(
        {
          ownerState: ownerState,
          get ["class"]() {
            return clsx(classes.select, props.class);
          },
          get disabled() {
            return props.disabled;
          },
        },
        other,
      ),
    ),
    createComponent(Show, {
      get when() {
        return !props.multiple;
      },
      get children() {
        return createComponent(NativeSelectIcon, {
          get as() {
            return props.IconComponent;
          },
          ownerState: ownerState,
          get ["class"]() {
            return classes.icon;
          },
        });
      },
    }),
  ];
};

const $$m = createComponentFactory()({
  name: "MuiNativeSelect",
  selfPropNames: [
    "children",
    "classes",
    "IconComponent",
    "input",
    "inputProps",
    "onChange",
    "value",
    "variant",
  ],
  utilityClass: getNativeSelectUtilityClasses,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const defaultInput = () => createComponent(Input, {});
/**
 * An alternative to `<Select native />` with a much smaller bundle size footprint.
 *
 * Demos:
 *
 * - [Selects](https://mui.com/components/selects/)
 *
 * API:
 *
 * - [NativeSelect API](https://mui.com/api/native-select/)
 * - inherits [Input API](https://mui.com/api/input/)
 */
const NativeSelect = $$m.defineComponent(function NativeSelect(inProps) {
  const props = $$m.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "class",
    "children",
    "classes",
    "IconComponent",
    "input",
    "inputProps",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      classes: {},
      IconComponent: ArrowDropDownIcon,
      input: defaultInput,
    },
    props,
  );
  if (baseProps.input !== defaultInput)
    // This feat requires component introspection (not supported by SolidJS)
    throw new Error(`NativeSelect 'input' custom property is not supported.`);
  const muiFormControl = useFormControl();
  const fcs = formControlState({
    props: props,
    muiFormControl: muiFormControl,
    states: ["variant"],
  });
  const ownerState = mergeProps(props, {
    get classes() {
      return baseProps.classes;
    },
  });
  const classes = $$m.useClasses(ownerState);
  const [, otherClasses] = splitProps(baseProps.classes, ["root"]);
  const inputProps = mergeProps(
    {
      get children() {
        return props.children;
      },
      classes: otherClasses,
      get IconComponent() {
        return baseProps.IconComponent;
      },
      get variant() {
        return fcs.variant;
      },
      type: undefined,
    },
    () => props.inputProps || {},
    //() => (baseProps.input ? baseProps.input.props.inputProps : {})
  );

  return createComponent(
    Input,
    mergeProps(
      {
        inputComponent: NativeSelectInput,
        inputProps: inputProps,
      },
      other,
      {
        get ["class"]() {
          return clsx(
            classes.root,
            //baseProps.input.props.className,
            props.class,
          );
        },
      },
    ),
  );
});

const _tmpl$$4 = /*#__PURE__*/ template(`<span>`),
  _tmpl$2 = /*#__PURE__*/ template(`<span class="notranslate">&#8203;`);
const $$l = createComponentFactory()({
  name: "MuiNotchedOutline",
  selfPropNames: ["disabled", "error", "focused", "label", "notched"],
});
const NotchedOutlineRoot$1 = styled$1("fieldset")({
  textAlign: "left",
  position: "absolute",
  bottom: 0,
  right: 0,
  top: -5,
  left: 0,
  margin: 0,
  padding: "0 8px",
  pointerEvents: "none",
  borderRadius: "inherit",
  borderStyle: "solid",
  borderWidth: 1,
  overflow: "hidden",
  minWidth: "0%",
});
const NotchedOutlineLegend = styled$1("legend")(({ ownerState, theme }) => ({
  float: "unset",
  // Fix conflict with bootstrap
  ...(!ownerState.withLabel && {
    padding: 0,
    lineHeight: "11px",
    // sync with `height` in `legend` styles
    transition: theme.transitions.create("width", {
      duration: 150,
      easing: theme.transitions.easing.easeOut,
    }),
  }),
  ...(ownerState.withLabel && {
    display: "block",
    // Fix conflict with normalize.css and sanitize.css
    width: "auto",
    // Fix conflict with bootstrap
    padding: 0,
    height: 11,
    // sync with `lineHeight` in `legend` styles
    fontSize: "0.75em",
    visibility: "hidden",
    maxWidth: 0.01,
    transition: theme.transitions.create("max-width", {
      duration: 50,
      easing: theme.transitions.easing.easeOut,
    }),
    whiteSpace: "nowrap",
    "& > span": {
      paddingLeft: 5,
      paddingRight: 5,
      display: "inline-block",
    },
    ...(ownerState.notched && {
      maxWidth: "100%",
      transition: theme.transitions.create("max-width", {
        duration: 100,
        easing: theme.transitions.easing.easeOut,
        delay: 50,
      }),
    }),
  }),
}));

/**
 * @ignore - internal component.
 */

const NotchedOutline = $$l.component(function NotchedOutline({
  allProps,
  otherProps,
  props,
}) {
  const withLabel = () => props.label != null && props.label !== "";
  const ownerState = mergeProps(allProps, {
    get withLabel() {
      return withLabel();
    },
  });
  return createComponent(
    NotchedOutlineRoot$1,
    mergeProps(otherProps, {
      "aria-hidden": true,
      get ["class"]() {
        return otherProps.class;
      },
      ownerState: ownerState,
      get children() {
        return createComponent(NotchedOutlineLegend, {
          ownerState: ownerState,
          get children() {
            return createComponent(Show, {
              get when() {
                return withLabel();
              },
              get fallback() {
                return (
                  // notranslate needed while Google Translate will not fix zero-width space issue
                  _tmpl$2()
                );
              },
              get children() {
                const _el$ = _tmpl$$4();
                insert(_el$, () => props.label);
                return _el$;
              },
            });
          },
        });
      },
    }),
  );
});

function getOutlinedInputUtilityClass(slot) {
  return generateUtilityClass("MuiOutlinedInput", slot);
}
const outlinedInputClasses = {
  ...inputBaseClasses,
  ...generateUtilityClasses("MuiOutlinedInput", [
    "root",
    "notchedOutline",
    "input",
  ]),
};

const $$k = createComponentFactory()({
  name: "MuiOutlinedInput",
  propDefaults: ({ set }) =>
    set({
      components: {},
      fullWidth: false,
      inputComponent: "input",
      multiline: false,
      type: "text",
    }),
  selfPropNames: ["classes", "label", "notched"],
  utilityClass: getOutlinedInputUtilityClass,
  slotClasses: () => ({
    root: ["root"],
    notchedOutline: ["notchedOutline"],
    input: ["input"],
  }),
});
const OutlinedInputRoot = styled$1(InputBaseRoot, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiOutlinedInput",
  slot: "Root",
  overridesResolver: rootOverridesResolver,
})(({ theme, ownerState }) => {
  const borderColor =
    theme.palette.mode === "light"
      ? "rgba(0, 0, 0, 0.23)"
      : "rgba(255, 255, 255, 0.23)";
  return {
    position: "relative",
    borderRadius: theme.shape.borderRadius,
    [`&:hover .${outlinedInputClasses.notchedOutline}`]: {
      borderColor: theme.palette.text.primary,
    },
    // Reset on touch devices, it doesn't add specificity
    "@media (hover: none)": {
      [`&:hover .${outlinedInputClasses.notchedOutline}`]: {
        borderColor,
      },
    },
    [`&.${outlinedInputClasses.focused} .${outlinedInputClasses.notchedOutline}`]:
      {
        // [review] ownerState.color is not required by MUI
        borderColor: theme.palette[ownerState.color].main,
        borderWidth: 2,
      },
    [`&.${outlinedInputClasses.error} .${outlinedInputClasses.notchedOutline}`]:
      {
        borderColor: theme.palette.error.main,
      },
    [`&.${outlinedInputClasses.disabled} .${outlinedInputClasses.notchedOutline}`]:
      {
        borderColor: theme.palette.action.disabled,
      },
    ...(ownerState.startAdornment && {
      paddingLeft: 14,
    }),
    ...(ownerState.endAdornment && {
      paddingRight: 14,
    }),
    ...(ownerState.multiline && {
      padding: "16.5px 14px",
      ...(ownerState.size === "small" && {
        padding: "8.5px 14px",
      }),
    }),
  };
});
const NotchedOutlineRoot = styled$1(NotchedOutline, {
  name: "MuiOutlinedInput",
  slot: "NotchedOutline",
  overridesResolver: (props, styles) => styles.notchedOutline,
})(({ theme }) => ({
  borderColor:
    theme.palette.mode === "light"
      ? "rgba(0, 0, 0, 0.23)"
      : "rgba(255, 255, 255, 0.23)",
}));
const OutlinedInputInput = styled$1(InputBaseComponent, {
  name: "MuiOutlinedInput",
  slot: "Input",
  overridesResolver: inputOverridesResolver,
})(({ theme, ownerState }) => ({
  padding: "16.5px 14px",
  "&:-webkit-autofill": {
    WebkitBoxShadow:
      theme.palette.mode === "light" ? null : "0 0 0 100px #266798 inset",
    WebkitTextFillColor: theme.palette.mode === "light" ? null : "#fff",
    caretColor: theme.palette.mode === "light" ? null : "#fff",
    borderRadius: "inherit",
  },
  ...(ownerState.size === "small" && {
    padding: "8.5px 14px",
  }),
  ...(ownerState.multiline && {
    padding: 0,
  }),
  ...(ownerState.startAdornment && {
    paddingLeft: 0,
  }),
  ...(ownerState.endAdornment && {
    paddingRight: 0,
  }),
}));
const OutlinedInput = $$k.component(function OutlinedInput({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const muiFormControl = useFormControl();
  const fcs = formControlState({
    props: allProps,
    muiFormControl,
    states: ["required"],
  });
  const label = createMemo(() => {
    const label = props.label;
    return label != null && label !== "" && fcs.required
      ? [label, "\xA0", "*"]
      : label;
  });
  const allClasses = mergeProps(classes, () => props.classes || {}, {
    notchedOutline: null,
  });
  return createComponent(
    InputBase,
    mergeProps(
      {
        renderSuffix: (state) =>
          createComponent(NotchedOutlineRoot, {
            get ["class"]() {
              return classes.notchedOutline;
            },
            get label() {
              return label();
            },
            get notched() {
              return createMemo(() => typeof props.notched !== "undefined")()
                ? props.notched
                : Boolean(
                    state.startAdornment || state.filled || state.focused,
                  );
            },
          }),
      },
      otherProps,
      {
        get components() {
          return {
            Root: OutlinedInputRoot,
            Input: OutlinedInputInput,
            ...(otherProps.components || {}),
          };
        },
        classes: allClasses,
      },
    ),
  );
});

var top = "top";
var bottom = "bottom";
var right = "right";
var left = "left";
var auto = "auto";
var basePlacements = [top, bottom, right, left];
var start = "start";
var end = "end";
var clippingParents = "clippingParents";
var viewport = "viewport";
var popper = "popper";
var reference = "reference";
var variationPlacements = /*#__PURE__*/ basePlacements.reduce(function (
  acc,
  placement,
) {
  return acc.concat([placement + "-" + start, placement + "-" + end]);
}, []);
var placements = /*#__PURE__*/ []
  .concat(basePlacements, [auto])
  .reduce(function (acc, placement) {
    return acc.concat([
      placement,
      placement + "-" + start,
      placement + "-" + end,
    ]);
  }, []); // modifiers that need to read the DOM

var beforeRead = "beforeRead";
var read = "read";
var afterRead = "afterRead"; // pure-logic modifiers

var beforeMain = "beforeMain";
var main = "main";
var afterMain = "afterMain"; // modifier with the purpose to write to the DOM (or write into a framework state)

var beforeWrite = "beforeWrite";
var write = "write";
var afterWrite = "afterWrite";
var modifierPhases = [
  beforeRead,
  read,
  afterRead,
  beforeMain,
  main,
  afterMain,
  beforeWrite,
  write,
  afterWrite,
];

function getNodeName(element) {
  return element ? (element.nodeName || "").toLowerCase() : null;
}

function getWindow(node) {
  if (node == null) {
    return window;
  }

  if (node.toString() !== "[object Window]") {
    var ownerDocument = node.ownerDocument;
    return ownerDocument ? ownerDocument.defaultView || window : window;
  }

  return node;
}

function isElement(node) {
  var OwnElement = getWindow(node).Element;
  return node instanceof OwnElement || node instanceof Element;
}

function isHTMLElement(node) {
  var OwnElement = getWindow(node).HTMLElement;
  return node instanceof OwnElement || node instanceof HTMLElement;
}

function isShadowRoot(node) {
  // IE 11 has no ShadowRoot
  if (typeof ShadowRoot === "undefined") {
    return false;
  }

  var OwnElement = getWindow(node).ShadowRoot;
  return node instanceof OwnElement || node instanceof ShadowRoot;
}

// and applies them to the HTMLElements such as popper and arrow

function applyStyles(_ref) {
  var state = _ref.state;
  Object.keys(state.elements).forEach(function (name) {
    var style = state.styles[name] || {};
    var attributes = state.attributes[name] || {};
    var element = state.elements[name]; // arrow is optional + virtual elements

    if (!isHTMLElement(element) || !getNodeName(element)) {
      return;
    } // Flow doesn't support to extend this property, but it's the most
    // effective way to apply styles to an HTMLElement
    // $FlowFixMe[cannot-write]

    Object.assign(element.style, style);
    Object.keys(attributes).forEach(function (name) {
      var value = attributes[name];

      if (value === false) {
        element.removeAttribute(name);
      } else {
        element.setAttribute(name, value === true ? "" : value);
      }
    });
  });
}

function effect$2(_ref2) {
  var state = _ref2.state;
  var initialStyles = {
    popper: {
      position: state.options.strategy,
      left: "0",
      top: "0",
      margin: "0",
    },
    arrow: {
      position: "absolute",
    },
    reference: {},
  };
  Object.assign(state.elements.popper.style, initialStyles.popper);
  state.styles = initialStyles;

  if (state.elements.arrow) {
    Object.assign(state.elements.arrow.style, initialStyles.arrow);
  }

  return function () {
    Object.keys(state.elements).forEach(function (name) {
      var element = state.elements[name];
      var attributes = state.attributes[name] || {};
      var styleProperties = Object.keys(
        state.styles.hasOwnProperty(name)
          ? state.styles[name]
          : initialStyles[name],
      ); // Set all values to an empty string to unset them

      var style = styleProperties.reduce(function (style, property) {
        style[property] = "";
        return style;
      }, {}); // arrow is optional + virtual elements

      if (!isHTMLElement(element) || !getNodeName(element)) {
        return;
      }

      Object.assign(element.style, style);
      Object.keys(attributes).forEach(function (attribute) {
        element.removeAttribute(attribute);
      });
    });
  };
} // eslint-disable-next-line import/no-unused-modules

const applyStyles$1 = {
  name: "applyStyles",
  enabled: true,
  phase: "write",
  fn: applyStyles,
  effect: effect$2,
  requires: ["computeStyles"],
};

function getBasePlacement(placement) {
  return placement.split("-")[0];
}

var max = Math.max;
var min = Math.min;
var round = Math.round;

function getUAString() {
  var uaData = navigator.userAgentData;

  if (uaData != null && uaData.brands && Array.isArray(uaData.brands)) {
    return uaData.brands
      .map(function (item) {
        return item.brand + "/" + item.version;
      })
      .join(" ");
  }

  return navigator.userAgent;
}

function isLayoutViewport() {
  return !/^((?!chrome|android).)*safari/i.test(getUAString());
}

function getBoundingClientRect(element, includeScale, isFixedStrategy) {
  if (includeScale === void 0) {
    includeScale = false;
  }

  if (isFixedStrategy === void 0) {
    isFixedStrategy = false;
  }

  var clientRect = element.getBoundingClientRect();
  var scaleX = 1;
  var scaleY = 1;

  if (includeScale && isHTMLElement(element)) {
    scaleX =
      element.offsetWidth > 0
        ? round(clientRect.width) / element.offsetWidth || 1
        : 1;
    scaleY =
      element.offsetHeight > 0
        ? round(clientRect.height) / element.offsetHeight || 1
        : 1;
  }

  var _ref = isElement(element) ? getWindow(element) : window,
    visualViewport = _ref.visualViewport;

  var addVisualOffsets = !isLayoutViewport() && isFixedStrategy;
  var x =
    (clientRect.left +
      (addVisualOffsets && visualViewport ? visualViewport.offsetLeft : 0)) /
    scaleX;
  var y =
    (clientRect.top +
      (addVisualOffsets && visualViewport ? visualViewport.offsetTop : 0)) /
    scaleY;
  var width = clientRect.width / scaleX;
  var height = clientRect.height / scaleY;
  return {
    width: width,
    height: height,
    top: y,
    right: x + width,
    bottom: y + height,
    left: x,
    x: x,
    y: y,
  };
}

// means it doesn't take into account transforms.

function getLayoutRect(element) {
  var clientRect = getBoundingClientRect(element); // Use the clientRect sizes if it's not been transformed.
  // Fixes https://github.com/popperjs/popper-core/issues/1223

  var width = element.offsetWidth;
  var height = element.offsetHeight;

  if (Math.abs(clientRect.width - width) <= 1) {
    width = clientRect.width;
  }

  if (Math.abs(clientRect.height - height) <= 1) {
    height = clientRect.height;
  }

  return {
    x: element.offsetLeft,
    y: element.offsetTop,
    width: width,
    height: height,
  };
}

function contains(parent, child) {
  var rootNode = child.getRootNode && child.getRootNode(); // First, attempt with faster native method

  if (parent.contains(child)) {
    return true;
  } // then fallback to custom implementation with Shadow DOM support
  else if (rootNode && isShadowRoot(rootNode)) {
    var next = child;

    do {
      if (next && parent.isSameNode(next)) {
        return true;
      } // $FlowFixMe[prop-missing]: need a better way to handle this...

      next = next.parentNode || next.host;
    } while (next);
  } // Give up, the result is false

  return false;
}

function getComputedStyle(element) {
  return getWindow(element).getComputedStyle(element);
}

function isTableElement(element) {
  return ["table", "td", "th"].indexOf(getNodeName(element)) >= 0;
}

function getDocumentElement(element) {
  // $FlowFixMe[incompatible-return]: assume body is always available
  return (
    (isElement(element)
      ? element.ownerDocument // $FlowFixMe[prop-missing]
      : element.document) || window.document
  ).documentElement;
}

function getParentNode(element) {
  if (getNodeName(element) === "html") {
    return element;
  }

  return (
    // this is a quicker (but less type safe) way to save quite some bytes from the bundle
    // $FlowFixMe[incompatible-return]
    // $FlowFixMe[prop-missing]
    element.assignedSlot || // step into the shadow DOM of the parent of a slotted node
    element.parentNode || // DOM Element detected
    (isShadowRoot(element) ? element.host : null) || // ShadowRoot detected
    // $FlowFixMe[incompatible-call]: HTMLElement is a Node
    getDocumentElement(element) // fallback
  );
}

function getTrueOffsetParent(element) {
  if (
    !isHTMLElement(element) || // https://github.com/popperjs/popper-core/issues/837
    getComputedStyle(element).position === "fixed"
  ) {
    return null;
  }

  return element.offsetParent;
} // `.offsetParent` reports `null` for fixed elements, while absolute elements
// return the containing block

function getContainingBlock(element) {
  var isFirefox = /firefox/i.test(getUAString());
  var isIE = /Trident/i.test(getUAString());

  if (isIE && isHTMLElement(element)) {
    // In IE 9, 10 and 11 fixed elements containing block is always established by the viewport
    var elementCss = getComputedStyle(element);

    if (elementCss.position === "fixed") {
      return null;
    }
  }

  var currentNode = getParentNode(element);

  if (isShadowRoot(currentNode)) {
    currentNode = currentNode.host;
  }

  while (
    isHTMLElement(currentNode) &&
    ["html", "body"].indexOf(getNodeName(currentNode)) < 0
  ) {
    var css = getComputedStyle(currentNode); // This is non-exhaustive but covers the most common CSS properties that
    // create a containing block.
    // https://developer.mozilla.org/en-US/docs/Web/CSS/Containing_block#identifying_the_containing_block

    if (
      css.transform !== "none" ||
      css.perspective !== "none" ||
      css.contain === "paint" ||
      ["transform", "perspective"].indexOf(css.willChange) !== -1 ||
      (isFirefox && css.willChange === "filter") ||
      (isFirefox && css.filter && css.filter !== "none")
    ) {
      return currentNode;
    } else {
      currentNode = currentNode.parentNode;
    }
  }

  return null;
} // Gets the closest ancestor positioned element. Handles some edge cases,
// such as table ancestors and cross browser bugs.

function getOffsetParent(element) {
  var window = getWindow(element);
  var offsetParent = getTrueOffsetParent(element);

  while (
    offsetParent &&
    isTableElement(offsetParent) &&
    getComputedStyle(offsetParent).position === "static"
  ) {
    offsetParent = getTrueOffsetParent(offsetParent);
  }

  if (
    offsetParent &&
    (getNodeName(offsetParent) === "html" ||
      (getNodeName(offsetParent) === "body" &&
        getComputedStyle(offsetParent).position === "static"))
  ) {
    return window;
  }

  return offsetParent || getContainingBlock(element) || window;
}

function getMainAxisFromPlacement(placement) {
  return ["top", "bottom"].indexOf(placement) >= 0 ? "x" : "y";
}

function within(min$1, value, max$1) {
  return max(min$1, min(value, max$1));
}
function withinMaxClamp(min, value, max) {
  var v = within(min, value, max);
  return v > max ? max : v;
}

function getFreshSideObject() {
  return {
    top: 0,
    right: 0,
    bottom: 0,
    left: 0,
  };
}

function mergePaddingObject(paddingObject) {
  return Object.assign({}, getFreshSideObject(), paddingObject);
}

function expandToHashMap(value, keys) {
  return keys.reduce(function (hashMap, key) {
    hashMap[key] = value;
    return hashMap;
  }, {});
}

var toPaddingObject = function toPaddingObject(padding, state) {
  padding =
    typeof padding === "function"
      ? padding(
          Object.assign({}, state.rects, {
            placement: state.placement,
          }),
        )
      : padding;
  return mergePaddingObject(
    typeof padding !== "number"
      ? padding
      : expandToHashMap(padding, basePlacements),
  );
};

function arrow(_ref) {
  var _state$modifiersData$;

  var state = _ref.state,
    name = _ref.name,
    options = _ref.options;
  var arrowElement = state.elements.arrow;
  var popperOffsets = state.modifiersData.popperOffsets;
  var basePlacement = getBasePlacement(state.placement);
  var axis = getMainAxisFromPlacement(basePlacement);
  var isVertical = [left, right].indexOf(basePlacement) >= 0;
  var len = isVertical ? "height" : "width";

  if (!arrowElement || !popperOffsets) {
    return;
  }

  var paddingObject = toPaddingObject(options.padding, state);
  var arrowRect = getLayoutRect(arrowElement);
  var minProp = axis === "y" ? top : left;
  var maxProp = axis === "y" ? bottom : right;
  var endDiff =
    state.rects.reference[len] +
    state.rects.reference[axis] -
    popperOffsets[axis] -
    state.rects.popper[len];
  var startDiff = popperOffsets[axis] - state.rects.reference[axis];
  var arrowOffsetParent = getOffsetParent(arrowElement);
  var clientSize = arrowOffsetParent
    ? axis === "y"
      ? arrowOffsetParent.clientHeight || 0
      : arrowOffsetParent.clientWidth || 0
    : 0;
  var centerToReference = endDiff / 2 - startDiff / 2; // Make sure the arrow doesn't overflow the popper if the center point is
  // outside of the popper bounds

  var min = paddingObject[minProp];
  var max = clientSize - arrowRect[len] - paddingObject[maxProp];
  var center = clientSize / 2 - arrowRect[len] / 2 + centerToReference;
  var offset = within(min, center, max); // Prevents breaking syntax highlighting...

  var axisProp = axis;
  state.modifiersData[name] =
    ((_state$modifiersData$ = {}),
    (_state$modifiersData$[axisProp] = offset),
    (_state$modifiersData$.centerOffset = offset - center),
    _state$modifiersData$);
}

function effect$1(_ref2) {
  var state = _ref2.state,
    options = _ref2.options;
  var _options$element = options.element,
    arrowElement =
      _options$element === void 0 ? "[data-popper-arrow]" : _options$element;

  if (arrowElement == null) {
    return;
  } // CSS selector

  if (typeof arrowElement === "string") {
    arrowElement = state.elements.popper.querySelector(arrowElement);

    if (!arrowElement) {
      return;
    }
  }

  if (!contains(state.elements.popper, arrowElement)) {
    return;
  }

  state.elements.arrow = arrowElement;
} // eslint-disable-next-line import/no-unused-modules

const arrow$1 = {
  name: "arrow",
  enabled: true,
  phase: "main",
  fn: arrow,
  effect: effect$1,
  requires: ["popperOffsets"],
  requiresIfExists: ["preventOverflow"],
};

function getVariation(placement) {
  return placement.split("-")[1];
}

var unsetSides = {
  top: "auto",
  right: "auto",
  bottom: "auto",
  left: "auto",
}; // Round the offsets to the nearest suitable subpixel based on the DPR.
// Zooming can change the DPR, but it seems to report a value that will
// cleanly divide the values into the appropriate subpixels.

function roundOffsetsByDPR(_ref, win) {
  var x = _ref.x,
    y = _ref.y;
  var dpr = win.devicePixelRatio || 1;
  return {
    x: round(x * dpr) / dpr || 0,
    y: round(y * dpr) / dpr || 0,
  };
}

function mapToStyles(_ref2) {
  var _Object$assign2;

  var popper = _ref2.popper,
    popperRect = _ref2.popperRect,
    placement = _ref2.placement,
    variation = _ref2.variation,
    offsets = _ref2.offsets,
    position = _ref2.position,
    gpuAcceleration = _ref2.gpuAcceleration,
    adaptive = _ref2.adaptive,
    roundOffsets = _ref2.roundOffsets,
    isFixed = _ref2.isFixed;
  var _offsets$x = offsets.x,
    x = _offsets$x === void 0 ? 0 : _offsets$x,
    _offsets$y = offsets.y,
    y = _offsets$y === void 0 ? 0 : _offsets$y;

  var _ref3 =
    typeof roundOffsets === "function"
      ? roundOffsets({
          x: x,
          y: y,
        })
      : {
          x: x,
          y: y,
        };

  x = _ref3.x;
  y = _ref3.y;
  var hasX = offsets.hasOwnProperty("x");
  var hasY = offsets.hasOwnProperty("y");
  var sideX = left;
  var sideY = top;
  var win = window;

  if (adaptive) {
    var offsetParent = getOffsetParent(popper);
    var heightProp = "clientHeight";
    var widthProp = "clientWidth";

    if (offsetParent === getWindow(popper)) {
      offsetParent = getDocumentElement(popper);

      if (
        getComputedStyle(offsetParent).position !== "static" &&
        position === "absolute"
      ) {
        heightProp = "scrollHeight";
        widthProp = "scrollWidth";
      }
    } // $FlowFixMe[incompatible-cast]: force type refinement, we compare offsetParent with window above, but Flow doesn't detect it

    offsetParent = offsetParent;

    if (
      placement === top ||
      ((placement === left || placement === right) && variation === end)
    ) {
      sideY = bottom;
      var offsetY =
        isFixed && offsetParent === win && win.visualViewport
          ? win.visualViewport.height // $FlowFixMe[prop-missing]
          : offsetParent[heightProp];
      y -= offsetY - popperRect.height;
      y *= gpuAcceleration ? 1 : -1;
    }

    if (
      placement === left ||
      ((placement === top || placement === bottom) && variation === end)
    ) {
      sideX = right;
      var offsetX =
        isFixed && offsetParent === win && win.visualViewport
          ? win.visualViewport.width // $FlowFixMe[prop-missing]
          : offsetParent[widthProp];
      x -= offsetX - popperRect.width;
      x *= gpuAcceleration ? 1 : -1;
    }
  }

  var commonStyles = Object.assign(
    {
      position: position,
    },
    adaptive && unsetSides,
  );

  var _ref4 =
    roundOffsets === true
      ? roundOffsetsByDPR(
          {
            x: x,
            y: y,
          },
          getWindow(popper),
        )
      : {
          x: x,
          y: y,
        };

  x = _ref4.x;
  y = _ref4.y;

  if (gpuAcceleration) {
    var _Object$assign;

    return Object.assign(
      {},
      commonStyles,
      ((_Object$assign = {}),
      (_Object$assign[sideY] = hasY ? "0" : ""),
      (_Object$assign[sideX] = hasX ? "0" : ""),
      (_Object$assign.transform =
        (win.devicePixelRatio || 1) <= 1
          ? "translate(" + x + "px, " + y + "px)"
          : "translate3d(" + x + "px, " + y + "px, 0)"),
      _Object$assign),
    );
  }

  return Object.assign(
    {},
    commonStyles,
    ((_Object$assign2 = {}),
    (_Object$assign2[sideY] = hasY ? y + "px" : ""),
    (_Object$assign2[sideX] = hasX ? x + "px" : ""),
    (_Object$assign2.transform = ""),
    _Object$assign2),
  );
}

function computeStyles(_ref5) {
  var state = _ref5.state,
    options = _ref5.options;
  var _options$gpuAccelerat = options.gpuAcceleration,
    gpuAcceleration =
      _options$gpuAccelerat === void 0 ? true : _options$gpuAccelerat,
    _options$adaptive = options.adaptive,
    adaptive = _options$adaptive === void 0 ? true : _options$adaptive,
    _options$roundOffsets = options.roundOffsets,
    roundOffsets =
      _options$roundOffsets === void 0 ? true : _options$roundOffsets;
  var commonStyles = {
    placement: getBasePlacement(state.placement),
    variation: getVariation(state.placement),
    popper: state.elements.popper,
    popperRect: state.rects.popper,
    gpuAcceleration: gpuAcceleration,
    isFixed: state.options.strategy === "fixed",
  };

  if (state.modifiersData.popperOffsets != null) {
    state.styles.popper = Object.assign(
      {},
      state.styles.popper,
      mapToStyles(
        Object.assign({}, commonStyles, {
          offsets: state.modifiersData.popperOffsets,
          position: state.options.strategy,
          adaptive: adaptive,
          roundOffsets: roundOffsets,
        }),
      ),
    );
  }

  if (state.modifiersData.arrow != null) {
    state.styles.arrow = Object.assign(
      {},
      state.styles.arrow,
      mapToStyles(
        Object.assign({}, commonStyles, {
          offsets: state.modifiersData.arrow,
          position: "absolute",
          adaptive: false,
          roundOffsets: roundOffsets,
        }),
      ),
    );
  }

  state.attributes.popper = Object.assign({}, state.attributes.popper, {
    "data-popper-placement": state.placement,
  });
} // eslint-disable-next-line import/no-unused-modules

const computeStyles$1 = {
  name: "computeStyles",
  enabled: true,
  phase: "beforeWrite",
  fn: computeStyles,
  data: {},
};

var passive = {
  passive: true,
};

function effect(_ref) {
  var state = _ref.state,
    instance = _ref.instance,
    options = _ref.options;
  var _options$scroll = options.scroll,
    scroll = _options$scroll === void 0 ? true : _options$scroll,
    _options$resize = options.resize,
    resize = _options$resize === void 0 ? true : _options$resize;
  var window = getWindow(state.elements.popper);
  var scrollParents = [].concat(
    state.scrollParents.reference,
    state.scrollParents.popper,
  );

  if (scroll) {
    scrollParents.forEach(function (scrollParent) {
      scrollParent.addEventListener("scroll", instance.update, passive);
    });
  }

  if (resize) {
    window.addEventListener("resize", instance.update, passive);
  }

  return function () {
    if (scroll) {
      scrollParents.forEach(function (scrollParent) {
        scrollParent.removeEventListener("scroll", instance.update, passive);
      });
    }

    if (resize) {
      window.removeEventListener("resize", instance.update, passive);
    }
  };
} // eslint-disable-next-line import/no-unused-modules

const eventListeners = {
  name: "eventListeners",
  enabled: true,
  phase: "write",
  fn: function fn() {},
  effect: effect,
  data: {},
};

var hash$1 = {
  left: "right",
  right: "left",
  bottom: "top",
  top: "bottom",
};
function getOppositePlacement(placement) {
  return placement.replace(/left|right|bottom|top/g, function (matched) {
    return hash$1[matched];
  });
}

var hash = {
  start: "end",
  end: "start",
};
function getOppositeVariationPlacement(placement) {
  return placement.replace(/start|end/g, function (matched) {
    return hash[matched];
  });
}

function getWindowScroll(node) {
  var win = getWindow(node);
  var scrollLeft = win.pageXOffset;
  var scrollTop = win.pageYOffset;
  return {
    scrollLeft: scrollLeft,
    scrollTop: scrollTop,
  };
}

function getWindowScrollBarX(element) {
  // If <html> has a CSS width greater than the viewport, then this will be
  // incorrect for RTL.
  // Popper 1 is broken in this case and never had a bug report so let's assume
  // it's not an issue. I don't think anyone ever specifies width on <html>
  // anyway.
  // Browsers where the left scrollbar doesn't cause an issue report `0` for
  // this (e.g. Edge 2019, IE11, Safari)
  return (
    getBoundingClientRect(getDocumentElement(element)).left +
    getWindowScroll(element).scrollLeft
  );
}

function getViewportRect(element, strategy) {
  var win = getWindow(element);
  var html = getDocumentElement(element);
  var visualViewport = win.visualViewport;
  var width = html.clientWidth;
  var height = html.clientHeight;
  var x = 0;
  var y = 0;

  if (visualViewport) {
    width = visualViewport.width;
    height = visualViewport.height;
    var layoutViewport = isLayoutViewport();

    if (layoutViewport || (!layoutViewport && strategy === "fixed")) {
      x = visualViewport.offsetLeft;
      y = visualViewport.offsetTop;
    }
  }

  return {
    width: width,
    height: height,
    x: x + getWindowScrollBarX(element),
    y: y,
  };
}

// of the `<html>` and `<body>` rect bounds if horizontally scrollable

function getDocumentRect(element) {
  var _element$ownerDocumen;

  var html = getDocumentElement(element);
  var winScroll = getWindowScroll(element);
  var body =
    (_element$ownerDocumen = element.ownerDocument) == null
      ? void 0
      : _element$ownerDocumen.body;
  var width = max(
    html.scrollWidth,
    html.clientWidth,
    body ? body.scrollWidth : 0,
    body ? body.clientWidth : 0,
  );
  var height = max(
    html.scrollHeight,
    html.clientHeight,
    body ? body.scrollHeight : 0,
    body ? body.clientHeight : 0,
  );
  var x = -winScroll.scrollLeft + getWindowScrollBarX(element);
  var y = -winScroll.scrollTop;

  if (getComputedStyle(body || html).direction === "rtl") {
    x += max(html.clientWidth, body ? body.clientWidth : 0) - width;
  }

  return {
    width: width,
    height: height,
    x: x,
    y: y,
  };
}

function isScrollParent(element) {
  // Firefox wants us to check `-x` and `-y` variations as well
  var _getComputedStyle = getComputedStyle(element),
    overflow = _getComputedStyle.overflow,
    overflowX = _getComputedStyle.overflowX,
    overflowY = _getComputedStyle.overflowY;

  return /auto|scroll|overlay|hidden/.test(overflow + overflowY + overflowX);
}

function getScrollParent(node) {
  if (["html", "body", "#document"].indexOf(getNodeName(node)) >= 0) {
    // $FlowFixMe[incompatible-return]: assume body is always available
    return node.ownerDocument.body;
  }

  if (isHTMLElement(node) && isScrollParent(node)) {
    return node;
  }

  return getScrollParent(getParentNode(node));
}

/*
given a DOM element, return the list of all scroll parents, up the list of ancesors
until we get to the top window object. This list is what we attach scroll listeners
to, because if any of these parent elements scroll, we'll need to re-calculate the
reference element's position.
*/

function listScrollParents(element, list) {
  var _element$ownerDocumen;

  if (list === void 0) {
    list = [];
  }

  var scrollParent = getScrollParent(element);
  var isBody =
    scrollParent ===
    ((_element$ownerDocumen = element.ownerDocument) == null
      ? void 0
      : _element$ownerDocumen.body);
  var win = getWindow(scrollParent);
  var target = isBody
    ? [win].concat(
        win.visualViewport || [],
        isScrollParent(scrollParent) ? scrollParent : [],
      )
    : scrollParent;
  var updatedList = list.concat(target);
  return isBody
    ? updatedList // $FlowFixMe[incompatible-call]: isBody tells us target will be an HTMLElement here
    : updatedList.concat(listScrollParents(getParentNode(target)));
}

function rectToClientRect(rect) {
  return Object.assign({}, rect, {
    left: rect.x,
    top: rect.y,
    right: rect.x + rect.width,
    bottom: rect.y + rect.height,
  });
}

function getInnerBoundingClientRect(element, strategy) {
  var rect = getBoundingClientRect(element, false, strategy === "fixed");
  rect.top = rect.top + element.clientTop;
  rect.left = rect.left + element.clientLeft;
  rect.bottom = rect.top + element.clientHeight;
  rect.right = rect.left + element.clientWidth;
  rect.width = element.clientWidth;
  rect.height = element.clientHeight;
  rect.x = rect.left;
  rect.y = rect.top;
  return rect;
}

function getClientRectFromMixedType(element, clippingParent, strategy) {
  return clippingParent === viewport
    ? rectToClientRect(getViewportRect(element, strategy))
    : isElement(clippingParent)
    ? getInnerBoundingClientRect(clippingParent, strategy)
    : rectToClientRect(getDocumentRect(getDocumentElement(element)));
} // A "clipping parent" is an overflowable container with the characteristic of
// clipping (or hiding) overflowing elements with a position different from
// `initial`

function getClippingParents(element) {
  var clippingParents = listScrollParents(getParentNode(element));
  var canEscapeClipping =
    ["absolute", "fixed"].indexOf(getComputedStyle(element).position) >= 0;
  var clipperElement =
    canEscapeClipping && isHTMLElement(element)
      ? getOffsetParent(element)
      : element;

  if (!isElement(clipperElement)) {
    return [];
  } // $FlowFixMe[incompatible-return]: https://github.com/facebook/flow/issues/1414

  return clippingParents.filter(function (clippingParent) {
    return (
      isElement(clippingParent) &&
      contains(clippingParent, clipperElement) &&
      getNodeName(clippingParent) !== "body"
    );
  });
} // Gets the maximum area that the element is visible in due to any number of
// clipping parents

function getClippingRect(element, boundary, rootBoundary, strategy) {
  var mainClippingParents =
    boundary === "clippingParents"
      ? getClippingParents(element)
      : [].concat(boundary);
  var clippingParents = [].concat(mainClippingParents, [rootBoundary]);
  var firstClippingParent = clippingParents[0];
  var clippingRect = clippingParents.reduce(
    function (accRect, clippingParent) {
      var rect = getClientRectFromMixedType(element, clippingParent, strategy);
      accRect.top = max(rect.top, accRect.top);
      accRect.right = min(rect.right, accRect.right);
      accRect.bottom = min(rect.bottom, accRect.bottom);
      accRect.left = max(rect.left, accRect.left);
      return accRect;
    },
    getClientRectFromMixedType(element, firstClippingParent, strategy),
  );
  clippingRect.width = clippingRect.right - clippingRect.left;
  clippingRect.height = clippingRect.bottom - clippingRect.top;
  clippingRect.x = clippingRect.left;
  clippingRect.y = clippingRect.top;
  return clippingRect;
}

function computeOffsets(_ref) {
  var reference = _ref.reference,
    element = _ref.element,
    placement = _ref.placement;
  var basePlacement = placement ? getBasePlacement(placement) : null;
  var variation = placement ? getVariation(placement) : null;
  var commonX = reference.x + reference.width / 2 - element.width / 2;
  var commonY = reference.y + reference.height / 2 - element.height / 2;
  var offsets;

  switch (basePlacement) {
    case top:
      offsets = {
        x: commonX,
        y: reference.y - element.height,
      };
      break;

    case bottom:
      offsets = {
        x: commonX,
        y: reference.y + reference.height,
      };
      break;

    case right:
      offsets = {
        x: reference.x + reference.width,
        y: commonY,
      };
      break;

    case left:
      offsets = {
        x: reference.x - element.width,
        y: commonY,
      };
      break;

    default:
      offsets = {
        x: reference.x,
        y: reference.y,
      };
  }

  var mainAxis = basePlacement ? getMainAxisFromPlacement(basePlacement) : null;

  if (mainAxis != null) {
    var len = mainAxis === "y" ? "height" : "width";

    switch (variation) {
      case start:
        offsets[mainAxis] =
          offsets[mainAxis] - (reference[len] / 2 - element[len] / 2);
        break;

      case end:
        offsets[mainAxis] =
          offsets[mainAxis] + (reference[len] / 2 - element[len] / 2);
        break;
    }
  }

  return offsets;
}

function detectOverflow(state, options) {
  if (options === void 0) {
    options = {};
  }

  var _options = options,
    _options$placement = _options.placement,
    placement =
      _options$placement === void 0 ? state.placement : _options$placement,
    _options$strategy = _options.strategy,
    strategy =
      _options$strategy === void 0 ? state.strategy : _options$strategy,
    _options$boundary = _options.boundary,
    boundary =
      _options$boundary === void 0 ? clippingParents : _options$boundary,
    _options$rootBoundary = _options.rootBoundary,
    rootBoundary =
      _options$rootBoundary === void 0 ? viewport : _options$rootBoundary,
    _options$elementConte = _options.elementContext,
    elementContext =
      _options$elementConte === void 0 ? popper : _options$elementConte,
    _options$altBoundary = _options.altBoundary,
    altBoundary =
      _options$altBoundary === void 0 ? false : _options$altBoundary,
    _options$padding = _options.padding,
    padding = _options$padding === void 0 ? 0 : _options$padding;
  var paddingObject = mergePaddingObject(
    typeof padding !== "number"
      ? padding
      : expandToHashMap(padding, basePlacements),
  );
  var altContext = elementContext === popper ? reference : popper;
  var popperRect = state.rects.popper;
  var element = state.elements[altBoundary ? altContext : elementContext];
  var clippingClientRect = getClippingRect(
    isElement(element)
      ? element
      : element.contextElement || getDocumentElement(state.elements.popper),
    boundary,
    rootBoundary,
    strategy,
  );
  var referenceClientRect = getBoundingClientRect(state.elements.reference);
  var popperOffsets = computeOffsets({
    reference: referenceClientRect,
    element: popperRect,
    strategy: "absolute",
    placement: placement,
  });
  var popperClientRect = rectToClientRect(
    Object.assign({}, popperRect, popperOffsets),
  );
  var elementClientRect =
    elementContext === popper ? popperClientRect : referenceClientRect; // positive = overflowing the clipping rect
  // 0 or negative = within the clipping rect

  var overflowOffsets = {
    top: clippingClientRect.top - elementClientRect.top + paddingObject.top,
    bottom:
      elementClientRect.bottom -
      clippingClientRect.bottom +
      paddingObject.bottom,
    left: clippingClientRect.left - elementClientRect.left + paddingObject.left,
    right:
      elementClientRect.right - clippingClientRect.right + paddingObject.right,
  };
  var offsetData = state.modifiersData.offset; // Offsets can be applied only to the popper element

  if (elementContext === popper && offsetData) {
    var offset = offsetData[placement];
    Object.keys(overflowOffsets).forEach(function (key) {
      var multiply = [right, bottom].indexOf(key) >= 0 ? 1 : -1;
      var axis = [top, bottom].indexOf(key) >= 0 ? "y" : "x";
      overflowOffsets[key] += offset[axis] * multiply;
    });
  }

  return overflowOffsets;
}

function computeAutoPlacement(state, options) {
  if (options === void 0) {
    options = {};
  }

  var _options = options,
    placement = _options.placement,
    boundary = _options.boundary,
    rootBoundary = _options.rootBoundary,
    padding = _options.padding,
    flipVariations = _options.flipVariations,
    _options$allowedAutoP = _options.allowedAutoPlacements,
    allowedAutoPlacements =
      _options$allowedAutoP === void 0 ? placements : _options$allowedAutoP;
  var variation = getVariation(placement);
  var placements$1 = variation
    ? flipVariations
      ? variationPlacements
      : variationPlacements.filter(function (placement) {
          return getVariation(placement) === variation;
        })
    : basePlacements;
  var allowedPlacements = placements$1.filter(function (placement) {
    return allowedAutoPlacements.indexOf(placement) >= 0;
  });

  if (allowedPlacements.length === 0) {
    allowedPlacements = placements$1;
  } // $FlowFixMe[incompatible-type]: Flow seems to have problems with two array unions...

  var overflows = allowedPlacements.reduce(function (acc, placement) {
    acc[placement] = detectOverflow(state, {
      placement: placement,
      boundary: boundary,
      rootBoundary: rootBoundary,
      padding: padding,
    })[getBasePlacement(placement)];
    return acc;
  }, {});
  return Object.keys(overflows).sort(function (a, b) {
    return overflows[a] - overflows[b];
  });
}

function getExpandedFallbackPlacements(placement) {
  if (getBasePlacement(placement) === auto) {
    return [];
  }

  var oppositePlacement = getOppositePlacement(placement);
  return [
    getOppositeVariationPlacement(placement),
    oppositePlacement,
    getOppositeVariationPlacement(oppositePlacement),
  ];
}

function flip(_ref) {
  var state = _ref.state,
    options = _ref.options,
    name = _ref.name;

  if (state.modifiersData[name]._skip) {
    return;
  }

  var _options$mainAxis = options.mainAxis,
    checkMainAxis = _options$mainAxis === void 0 ? true : _options$mainAxis,
    _options$altAxis = options.altAxis,
    checkAltAxis = _options$altAxis === void 0 ? true : _options$altAxis,
    specifiedFallbackPlacements = options.fallbackPlacements,
    padding = options.padding,
    boundary = options.boundary,
    rootBoundary = options.rootBoundary,
    altBoundary = options.altBoundary,
    _options$flipVariatio = options.flipVariations,
    flipVariations =
      _options$flipVariatio === void 0 ? true : _options$flipVariatio,
    allowedAutoPlacements = options.allowedAutoPlacements;
  var preferredPlacement = state.options.placement;
  var basePlacement = getBasePlacement(preferredPlacement);
  var isBasePlacement = basePlacement === preferredPlacement;
  var fallbackPlacements =
    specifiedFallbackPlacements ||
    (isBasePlacement || !flipVariations
      ? [getOppositePlacement(preferredPlacement)]
      : getExpandedFallbackPlacements(preferredPlacement));
  var placements = [preferredPlacement]
    .concat(fallbackPlacements)
    .reduce(function (acc, placement) {
      return acc.concat(
        getBasePlacement(placement) === auto
          ? computeAutoPlacement(state, {
              placement: placement,
              boundary: boundary,
              rootBoundary: rootBoundary,
              padding: padding,
              flipVariations: flipVariations,
              allowedAutoPlacements: allowedAutoPlacements,
            })
          : placement,
      );
    }, []);
  var referenceRect = state.rects.reference;
  var popperRect = state.rects.popper;
  var checksMap = new Map();
  var makeFallbackChecks = true;
  var firstFittingPlacement = placements[0];

  for (var i = 0; i < placements.length; i++) {
    var placement = placements[i];

    var _basePlacement = getBasePlacement(placement);

    var isStartVariation = getVariation(placement) === start;
    var isVertical = [top, bottom].indexOf(_basePlacement) >= 0;
    var len = isVertical ? "width" : "height";
    var overflow = detectOverflow(state, {
      placement: placement,
      boundary: boundary,
      rootBoundary: rootBoundary,
      altBoundary: altBoundary,
      padding: padding,
    });
    var mainVariationSide = isVertical
      ? isStartVariation
        ? right
        : left
      : isStartVariation
      ? bottom
      : top;

    if (referenceRect[len] > popperRect[len]) {
      mainVariationSide = getOppositePlacement(mainVariationSide);
    }

    var altVariationSide = getOppositePlacement(mainVariationSide);
    var checks = [];

    if (checkMainAxis) {
      checks.push(overflow[_basePlacement] <= 0);
    }

    if (checkAltAxis) {
      checks.push(
        overflow[mainVariationSide] <= 0,
        overflow[altVariationSide] <= 0,
      );
    }

    if (
      checks.every(function (check) {
        return check;
      })
    ) {
      firstFittingPlacement = placement;
      makeFallbackChecks = false;
      break;
    }

    checksMap.set(placement, checks);
  }

  if (makeFallbackChecks) {
    // `2` may be desired in some cases – research later
    var numberOfChecks = flipVariations ? 3 : 1;

    var _loop = function _loop(_i) {
      var fittingPlacement = placements.find(function (placement) {
        var checks = checksMap.get(placement);

        if (checks) {
          return checks.slice(0, _i).every(function (check) {
            return check;
          });
        }
      });

      if (fittingPlacement) {
        firstFittingPlacement = fittingPlacement;
        return "break";
      }
    };

    for (var _i = numberOfChecks; _i > 0; _i--) {
      var _ret = _loop(_i);

      if (_ret === "break") break;
    }
  }

  if (state.placement !== firstFittingPlacement) {
    state.modifiersData[name]._skip = true;
    state.placement = firstFittingPlacement;
    state.reset = true;
  }
} // eslint-disable-next-line import/no-unused-modules

const flip$1 = {
  name: "flip",
  enabled: true,
  phase: "main",
  fn: flip,
  requiresIfExists: ["offset"],
  data: {
    _skip: false,
  },
};

function getSideOffsets(overflow, rect, preventedOffsets) {
  if (preventedOffsets === void 0) {
    preventedOffsets = {
      x: 0,
      y: 0,
    };
  }

  return {
    top: overflow.top - rect.height - preventedOffsets.y,
    right: overflow.right - rect.width + preventedOffsets.x,
    bottom: overflow.bottom - rect.height + preventedOffsets.y,
    left: overflow.left - rect.width - preventedOffsets.x,
  };
}

function isAnySideFullyClipped(overflow) {
  return [top, right, bottom, left].some(function (side) {
    return overflow[side] >= 0;
  });
}

function hide(_ref) {
  var state = _ref.state,
    name = _ref.name;
  var referenceRect = state.rects.reference;
  var popperRect = state.rects.popper;
  var preventedOffsets = state.modifiersData.preventOverflow;
  var referenceOverflow = detectOverflow(state, {
    elementContext: "reference",
  });
  var popperAltOverflow = detectOverflow(state, {
    altBoundary: true,
  });
  var referenceClippingOffsets = getSideOffsets(
    referenceOverflow,
    referenceRect,
  );
  var popperEscapeOffsets = getSideOffsets(
    popperAltOverflow,
    popperRect,
    preventedOffsets,
  );
  var isReferenceHidden = isAnySideFullyClipped(referenceClippingOffsets);
  var hasPopperEscaped = isAnySideFullyClipped(popperEscapeOffsets);
  state.modifiersData[name] = {
    referenceClippingOffsets: referenceClippingOffsets,
    popperEscapeOffsets: popperEscapeOffsets,
    isReferenceHidden: isReferenceHidden,
    hasPopperEscaped: hasPopperEscaped,
  };
  state.attributes.popper = Object.assign({}, state.attributes.popper, {
    "data-popper-reference-hidden": isReferenceHidden,
    "data-popper-escaped": hasPopperEscaped,
  });
} // eslint-disable-next-line import/no-unused-modules

const hide$1 = {
  name: "hide",
  enabled: true,
  phase: "main",
  requiresIfExists: ["preventOverflow"],
  fn: hide,
};

function distanceAndSkiddingToXY(placement, rects, offset) {
  var basePlacement = getBasePlacement(placement);
  var invertDistance = [left, top].indexOf(basePlacement) >= 0 ? -1 : 1;

  var _ref =
      typeof offset === "function"
        ? offset(
            Object.assign({}, rects, {
              placement: placement,
            }),
          )
        : offset,
    skidding = _ref[0],
    distance = _ref[1];

  skidding = skidding || 0;
  distance = (distance || 0) * invertDistance;
  return [left, right].indexOf(basePlacement) >= 0
    ? {
        x: distance,
        y: skidding,
      }
    : {
        x: skidding,
        y: distance,
      };
}

function offset(_ref2) {
  var state = _ref2.state,
    options = _ref2.options,
    name = _ref2.name;
  var _options$offset = options.offset,
    offset = _options$offset === void 0 ? [0, 0] : _options$offset;
  var data = placements.reduce(function (acc, placement) {
    acc[placement] = distanceAndSkiddingToXY(placement, state.rects, offset);
    return acc;
  }, {});
  var _data$state$placement = data[state.placement],
    x = _data$state$placement.x,
    y = _data$state$placement.y;

  if (state.modifiersData.popperOffsets != null) {
    state.modifiersData.popperOffsets.x += x;
    state.modifiersData.popperOffsets.y += y;
  }

  state.modifiersData[name] = data;
} // eslint-disable-next-line import/no-unused-modules

const offset$1 = {
  name: "offset",
  enabled: true,
  phase: "main",
  requires: ["popperOffsets"],
  fn: offset,
};

function popperOffsets(_ref) {
  var state = _ref.state,
    name = _ref.name;
  // Offsets are the actual position the popper needs to have to be
  // properly positioned near its reference element
  // This is the most basic placement, and will be adjusted by
  // the modifiers in the next step
  state.modifiersData[name] = computeOffsets({
    reference: state.rects.reference,
    element: state.rects.popper,
    strategy: "absolute",
    placement: state.placement,
  });
} // eslint-disable-next-line import/no-unused-modules

const popperOffsets$1 = {
  name: "popperOffsets",
  enabled: true,
  phase: "read",
  fn: popperOffsets,
  data: {},
};

function getAltAxis(axis) {
  return axis === "x" ? "y" : "x";
}

function preventOverflow(_ref) {
  var state = _ref.state,
    options = _ref.options,
    name = _ref.name;
  var _options$mainAxis = options.mainAxis,
    checkMainAxis = _options$mainAxis === void 0 ? true : _options$mainAxis,
    _options$altAxis = options.altAxis,
    checkAltAxis = _options$altAxis === void 0 ? false : _options$altAxis,
    boundary = options.boundary,
    rootBoundary = options.rootBoundary,
    altBoundary = options.altBoundary,
    padding = options.padding,
    _options$tether = options.tether,
    tether = _options$tether === void 0 ? true : _options$tether,
    _options$tetherOffset = options.tetherOffset,
    tetherOffset = _options$tetherOffset === void 0 ? 0 : _options$tetherOffset;
  var overflow = detectOverflow(state, {
    boundary: boundary,
    rootBoundary: rootBoundary,
    padding: padding,
    altBoundary: altBoundary,
  });
  var basePlacement = getBasePlacement(state.placement);
  var variation = getVariation(state.placement);
  var isBasePlacement = !variation;
  var mainAxis = getMainAxisFromPlacement(basePlacement);
  var altAxis = getAltAxis(mainAxis);
  var popperOffsets = state.modifiersData.popperOffsets;
  var referenceRect = state.rects.reference;
  var popperRect = state.rects.popper;
  var tetherOffsetValue =
    typeof tetherOffset === "function"
      ? tetherOffset(
          Object.assign({}, state.rects, {
            placement: state.placement,
          }),
        )
      : tetherOffset;
  var normalizedTetherOffsetValue =
    typeof tetherOffsetValue === "number"
      ? {
          mainAxis: tetherOffsetValue,
          altAxis: tetherOffsetValue,
        }
      : Object.assign(
          {
            mainAxis: 0,
            altAxis: 0,
          },
          tetherOffsetValue,
        );
  var offsetModifierState = state.modifiersData.offset
    ? state.modifiersData.offset[state.placement]
    : null;
  var data = {
    x: 0,
    y: 0,
  };

  if (!popperOffsets) {
    return;
  }

  if (checkMainAxis) {
    var _offsetModifierState$;

    var mainSide = mainAxis === "y" ? top : left;
    var altSide = mainAxis === "y" ? bottom : right;
    var len = mainAxis === "y" ? "height" : "width";
    var offset = popperOffsets[mainAxis];
    var min$1 = offset + overflow[mainSide];
    var max$1 = offset - overflow[altSide];
    var additive = tether ? -popperRect[len] / 2 : 0;
    var minLen = variation === start ? referenceRect[len] : popperRect[len];
    var maxLen = variation === start ? -popperRect[len] : -referenceRect[len]; // We need to include the arrow in the calculation so the arrow doesn't go
    // outside the reference bounds

    var arrowElement = state.elements.arrow;
    var arrowRect =
      tether && arrowElement
        ? getLayoutRect(arrowElement)
        : {
            width: 0,
            height: 0,
          };
    var arrowPaddingObject = state.modifiersData["arrow#persistent"]
      ? state.modifiersData["arrow#persistent"].padding
      : getFreshSideObject();
    var arrowPaddingMin = arrowPaddingObject[mainSide];
    var arrowPaddingMax = arrowPaddingObject[altSide]; // If the reference length is smaller than the arrow length, we don't want
    // to include its full size in the calculation. If the reference is small
    // and near the edge of a boundary, the popper can overflow even if the
    // reference is not overflowing as well (e.g. virtual elements with no
    // width or height)

    var arrowLen = within(0, referenceRect[len], arrowRect[len]);
    var minOffset = isBasePlacement
      ? referenceRect[len] / 2 -
        additive -
        arrowLen -
        arrowPaddingMin -
        normalizedTetherOffsetValue.mainAxis
      : minLen -
        arrowLen -
        arrowPaddingMin -
        normalizedTetherOffsetValue.mainAxis;
    var maxOffset = isBasePlacement
      ? -referenceRect[len] / 2 +
        additive +
        arrowLen +
        arrowPaddingMax +
        normalizedTetherOffsetValue.mainAxis
      : maxLen +
        arrowLen +
        arrowPaddingMax +
        normalizedTetherOffsetValue.mainAxis;
    var arrowOffsetParent =
      state.elements.arrow && getOffsetParent(state.elements.arrow);
    var clientOffset = arrowOffsetParent
      ? mainAxis === "y"
        ? arrowOffsetParent.clientTop || 0
        : arrowOffsetParent.clientLeft || 0
      : 0;
    var offsetModifierValue =
      (_offsetModifierState$ =
        offsetModifierState == null ? void 0 : offsetModifierState[mainAxis]) !=
      null
        ? _offsetModifierState$
        : 0;
    var tetherMin = offset + minOffset - offsetModifierValue - clientOffset;
    var tetherMax = offset + maxOffset - offsetModifierValue;
    var preventedOffset = within(
      tether ? min(min$1, tetherMin) : min$1,
      offset,
      tether ? max(max$1, tetherMax) : max$1,
    );
    popperOffsets[mainAxis] = preventedOffset;
    data[mainAxis] = preventedOffset - offset;
  }

  if (checkAltAxis) {
    var _offsetModifierState$2;

    var _mainSide = mainAxis === "x" ? top : left;

    var _altSide = mainAxis === "x" ? bottom : right;

    var _offset = popperOffsets[altAxis];

    var _len = altAxis === "y" ? "height" : "width";

    var _min = _offset + overflow[_mainSide];

    var _max = _offset - overflow[_altSide];

    var isOriginSide = [top, left].indexOf(basePlacement) !== -1;

    var _offsetModifierValue =
      (_offsetModifierState$2 =
        offsetModifierState == null ? void 0 : offsetModifierState[altAxis]) !=
      null
        ? _offsetModifierState$2
        : 0;

    var _tetherMin = isOriginSide
      ? _min
      : _offset -
        referenceRect[_len] -
        popperRect[_len] -
        _offsetModifierValue +
        normalizedTetherOffsetValue.altAxis;

    var _tetherMax = isOriginSide
      ? _offset +
        referenceRect[_len] +
        popperRect[_len] -
        _offsetModifierValue -
        normalizedTetherOffsetValue.altAxis
      : _max;

    var _preventedOffset =
      tether && isOriginSide
        ? withinMaxClamp(_tetherMin, _offset, _tetherMax)
        : within(
            tether ? _tetherMin : _min,
            _offset,
            tether ? _tetherMax : _max,
          );

    popperOffsets[altAxis] = _preventedOffset;
    data[altAxis] = _preventedOffset - _offset;
  }

  state.modifiersData[name] = data;
} // eslint-disable-next-line import/no-unused-modules

const preventOverflow$1 = {
  name: "preventOverflow",
  enabled: true,
  phase: "main",
  fn: preventOverflow,
  requiresIfExists: ["offset"],
};

function getHTMLElementScroll(element) {
  return {
    scrollLeft: element.scrollLeft,
    scrollTop: element.scrollTop,
  };
}

function getNodeScroll(node) {
  if (node === getWindow(node) || !isHTMLElement(node)) {
    return getWindowScroll(node);
  } else {
    return getHTMLElementScroll(node);
  }
}

function isElementScaled(element) {
  var rect = element.getBoundingClientRect();
  var scaleX = round(rect.width) / element.offsetWidth || 1;
  var scaleY = round(rect.height) / element.offsetHeight || 1;
  return scaleX !== 1 || scaleY !== 1;
} // Returns the composite rect of an element relative to its offsetParent.
// Composite means it takes into account transforms as well as layout.

function getCompositeRect(elementOrVirtualElement, offsetParent, isFixed) {
  if (isFixed === void 0) {
    isFixed = false;
  }

  var isOffsetParentAnElement = isHTMLElement(offsetParent);
  var offsetParentIsScaled =
    isHTMLElement(offsetParent) && isElementScaled(offsetParent);
  var documentElement = getDocumentElement(offsetParent);
  var rect = getBoundingClientRect(
    elementOrVirtualElement,
    offsetParentIsScaled,
    isFixed,
  );
  var scroll = {
    scrollLeft: 0,
    scrollTop: 0,
  };
  var offsets = {
    x: 0,
    y: 0,
  };

  if (isOffsetParentAnElement || (!isOffsetParentAnElement && !isFixed)) {
    if (
      getNodeName(offsetParent) !== "body" || // https://github.com/popperjs/popper-core/issues/1078
      isScrollParent(documentElement)
    ) {
      scroll = getNodeScroll(offsetParent);
    }

    if (isHTMLElement(offsetParent)) {
      offsets = getBoundingClientRect(offsetParent, true);
      offsets.x += offsetParent.clientLeft;
      offsets.y += offsetParent.clientTop;
    } else if (documentElement) {
      offsets.x = getWindowScrollBarX(documentElement);
    }
  }

  return {
    x: rect.left + scroll.scrollLeft - offsets.x,
    y: rect.top + scroll.scrollTop - offsets.y,
    width: rect.width,
    height: rect.height,
  };
}

function order(modifiers) {
  var map = new Map();
  var visited = new Set();
  var result = [];
  modifiers.forEach(function (modifier) {
    map.set(modifier.name, modifier);
  }); // On visiting object, check for its dependencies and visit them recursively

  function sort(modifier) {
    visited.add(modifier.name);
    var requires = [].concat(
      modifier.requires || [],
      modifier.requiresIfExists || [],
    );
    requires.forEach(function (dep) {
      if (!visited.has(dep)) {
        var depModifier = map.get(dep);

        if (depModifier) {
          sort(depModifier);
        }
      }
    });
    result.push(modifier);
  }

  modifiers.forEach(function (modifier) {
    if (!visited.has(modifier.name)) {
      // check for visited object
      sort(modifier);
    }
  });
  return result;
}

function orderModifiers(modifiers) {
  // order based on dependencies
  var orderedModifiers = order(modifiers); // order based on phase

  return modifierPhases.reduce(function (acc, phase) {
    return acc.concat(
      orderedModifiers.filter(function (modifier) {
        return modifier.phase === phase;
      }),
    );
  }, []);
}

function debounce(fn) {
  var pending;
  return function () {
    if (!pending) {
      pending = new Promise(function (resolve) {
        Promise.resolve().then(function () {
          pending = undefined;
          resolve(fn());
        });
      });
    }

    return pending;
  };
}

function mergeByName(modifiers) {
  var merged = modifiers.reduce(function (merged, current) {
    var existing = merged[current.name];
    merged[current.name] = existing
      ? Object.assign({}, existing, current, {
          options: Object.assign({}, existing.options, current.options),
          data: Object.assign({}, existing.data, current.data),
        })
      : current;
    return merged;
  }, {}); // IE11 does not support Object.values

  return Object.keys(merged).map(function (key) {
    return merged[key];
  });
}

var DEFAULT_OPTIONS = {
  placement: "bottom",
  modifiers: [],
  strategy: "absolute",
};

function areValidElements() {
  for (
    var _len = arguments.length, args = new Array(_len), _key = 0;
    _key < _len;
    _key++
  ) {
    args[_key] = arguments[_key];
  }

  return !args.some(function (element) {
    return !(element && typeof element.getBoundingClientRect === "function");
  });
}

function popperGenerator(generatorOptions) {
  if (generatorOptions === void 0) {
    generatorOptions = {};
  }

  var _generatorOptions = generatorOptions,
    _generatorOptions$def = _generatorOptions.defaultModifiers,
    defaultModifiers =
      _generatorOptions$def === void 0 ? [] : _generatorOptions$def,
    _generatorOptions$def2 = _generatorOptions.defaultOptions,
    defaultOptions =
      _generatorOptions$def2 === void 0
        ? DEFAULT_OPTIONS
        : _generatorOptions$def2;
  return function createPopper(reference, popper, options) {
    if (options === void 0) {
      options = defaultOptions;
    }

    var state = {
      placement: "bottom",
      orderedModifiers: [],
      options: Object.assign({}, DEFAULT_OPTIONS, defaultOptions),
      modifiersData: {},
      elements: {
        reference: reference,
        popper: popper,
      },
      attributes: {},
      styles: {},
    };
    var effectCleanupFns = [];
    var isDestroyed = false;
    var instance = {
      state: state,
      setOptions: function setOptions(setOptionsAction) {
        var options =
          typeof setOptionsAction === "function"
            ? setOptionsAction(state.options)
            : setOptionsAction;
        cleanupModifierEffects();
        state.options = Object.assign(
          {},
          defaultOptions,
          state.options,
          options,
        );
        state.scrollParents = {
          reference: isElement(reference)
            ? listScrollParents(reference)
            : reference.contextElement
            ? listScrollParents(reference.contextElement)
            : [],
          popper: listScrollParents(popper),
        }; // Orders the modifiers based on their dependencies and `phase`
        // properties

        var orderedModifiers = orderModifiers(
          mergeByName([].concat(defaultModifiers, state.options.modifiers)),
        ); // Strip out disabled modifiers

        state.orderedModifiers = orderedModifiers.filter(function (m) {
          return m.enabled;
        });
        runModifierEffects();
        return instance.update();
      },
      // Sync update – it will always be executed, even if not necessary. This
      // is useful for low frequency updates where sync behavior simplifies the
      // logic.
      // For high frequency updates (e.g. `resize` and `scroll` events), always
      // prefer the async Popper#update method
      forceUpdate: function forceUpdate() {
        if (isDestroyed) {
          return;
        }

        var _state$elements = state.elements,
          reference = _state$elements.reference,
          popper = _state$elements.popper; // Don't proceed if `reference` or `popper` are not valid elements
        // anymore

        if (!areValidElements(reference, popper)) {
          return;
        } // Store the reference and popper rects to be read by modifiers

        state.rects = {
          reference: getCompositeRect(
            reference,
            getOffsetParent(popper),
            state.options.strategy === "fixed",
          ),
          popper: getLayoutRect(popper),
        }; // Modifiers have the ability to reset the current update cycle. The
        // most common use case for this is the `flip` modifier changing the
        // placement, which then needs to re-run all the modifiers, because the
        // logic was previously ran for the previous placement and is therefore
        // stale/incorrect

        state.reset = false;
        state.placement = state.options.placement; // On each update cycle, the `modifiersData` property for each modifier
        // is filled with the initial data specified by the modifier. This means
        // it doesn't persist and is fresh on each update.
        // To ensure persistent data, use `${name}#persistent`

        state.orderedModifiers.forEach(function (modifier) {
          return (state.modifiersData[modifier.name] = Object.assign(
            {},
            modifier.data,
          ));
        });

        for (var index = 0; index < state.orderedModifiers.length; index++) {
          if (state.reset === true) {
            state.reset = false;
            index = -1;
            continue;
          }

          var _state$orderedModifie = state.orderedModifiers[index],
            fn = _state$orderedModifie.fn,
            _state$orderedModifie2 = _state$orderedModifie.options,
            _options =
              _state$orderedModifie2 === void 0 ? {} : _state$orderedModifie2,
            name = _state$orderedModifie.name;

          if (typeof fn === "function") {
            state =
              fn({
                state: state,
                options: _options,
                name: name,
                instance: instance,
              }) || state;
          }
        }
      },
      // Async and optimistically optimized update – it will not be executed if
      // not necessary (debounced to run at most once-per-tick)
      update: debounce(function () {
        return new Promise(function (resolve) {
          instance.forceUpdate();
          resolve(state);
        });
      }),
      destroy: function destroy() {
        cleanupModifierEffects();
        isDestroyed = true;
      },
    };

    if (!areValidElements(reference, popper)) {
      return instance;
    }

    instance.setOptions(options).then(function (state) {
      if (!isDestroyed && options.onFirstUpdate) {
        options.onFirstUpdate(state);
      }
    }); // Modifiers have the ability to execute arbitrary code before the first
    // update cycle runs. They will be executed in the same order as the update
    // cycle. This is useful when a modifier adds some persistent data that
    // other modifiers need to use, but the modifier is run after the dependent
    // one.

    function runModifierEffects() {
      state.orderedModifiers.forEach(function (_ref) {
        var name = _ref.name,
          _ref$options = _ref.options,
          options = _ref$options === void 0 ? {} : _ref$options,
          effect = _ref.effect;

        if (typeof effect === "function") {
          var cleanupFn = effect({
            state: state,
            name: name,
            instance: instance,
            options: options,
          });

          var noopFn = function noopFn() {};

          effectCleanupFns.push(cleanupFn || noopFn);
        }
      });
    }

    function cleanupModifierEffects() {
      effectCleanupFns.forEach(function (fn) {
        return fn();
      });
      effectCleanupFns = [];
    }

    return instance;
  };
}

var defaultModifiers = [
  eventListeners,
  popperOffsets$1,
  computeStyles$1,
  applyStyles$1,
  offset$1,
  flip$1,
  preventOverflow$1,
  arrow$1,
  hide$1,
];
var createPopper = /*#__PURE__*/ popperGenerator({
  defaultModifiers: defaultModifiers,
}); // eslint-disable-next-line import/no-unused-modules

const _tmpl$$3 = /*#__PURE__*/ template(`<div role="tooltip">`);
const $$j = createComponentFactory()({
  name: "MuiPopperUnstyled",
  selfPropNames: [
    "anchorEl",
    "children",
    "container",
    "direction",
    "disablePortal",
    "keepMounted",
    "modifiers",
    "open",
    "placement",
    "popperOptions",
    "popperRef",
    "ref",
    "transition",
  ],
  propDefaults: ({ set }) =>
    set({
      direction: "ltr",
      disablePortal: false,
      keepMounted: false,
      placement: "bottom",
      transition: false,
      popperOptions: {},
    }),
});
function flipPlacement(placement, direction) {
  if (direction === "ltr") {
    return placement;
  }
  switch (placement) {
    case "bottom-end":
      return "bottom-start";
    case "bottom-start":
      return "bottom-end";
    case "top-end":
      return "top-start";
    case "top-start":
      return "top-end";
    default:
      return placement;
  }
}
function resolveAnchorEl(anchorEl) {
  return typeof anchorEl === "function" ? anchorEl() : anchorEl;
}
const PopperTooltip = function PopperTooltip(inProps) {
  const [props, otherProps] = splitProps(inProps, [
    "anchorEl",
    "children",
    "direction",
    "disablePortal",
    "modifiers",
    "open",
    //"ownerState",
    "TransitionProps",
    "placement",
    "popperOptions",
    "popperRef",
  ]);
  const rtlPlacement = flipPlacement(props.placement, props.direction);
  /**
   * placement initialized from prop but can change during lifetime if modifiers.flip.
   * modifiers.flip is essentially a flip for controlled/uncontrolled behavior
   */
  const [placement, setPlacement] = createSignal(rtlPlacement);
  let destructors = [];
  const tooltip = createElementRef();
  const popperInstance = createRef();
  createEffect((prevDestructor) => {
    if (prevDestructor) {
      destructors = destructors.filter((v) => v !== prevDestructor);
      prevDestructor();
    }
    if (!props.anchorEl || !props.open) {
      return undefined;
    }
    resolveAnchorEl(props.anchorEl);
    let popperModifiers = [
      {
        name: "preventOverflow",
        options: {
          altBoundary: props.disablePortal,
        },
      },
      {
        name: "flip",
        options: {
          altBoundary: props.disablePortal,
        },
      },
      {
        name: "onUpdate",
        enabled: true,
        phase: "afterWrite",
        fn: ({ state }) => {
          setPlacement(state.placement);
        },
      },
    ];
    if (props.modifiers != null) {
      popperModifiers = popperModifiers.concat(props.modifiers);
    }
    if (props.popperOptions && props.popperOptions.modifiers != null) {
      popperModifiers = popperModifiers.concat(props.popperOptions.modifiers);
    }
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const popper = createPopper(resolveAnchorEl(props.anchorEl), tooltip.ref, {
      placement: rtlPlacement,
      ...props.popperOptions,
      modifiers: popperModifiers,
    });
    popperInstance(popper);
    const destructor = () => {
      popper.destroy();
      popperInstance(null);
    };
    destructors.push(destructor);
    return destructor;
  });
  onMount(() => {
    if (popperInstance.ref) {
      popperInstance.ref.forceUpdate();
    }
  });
  const childProps = createMemo(() => ({
    placement: placement(),
    TransitionProps: props.TransitionProps,
  }));
  return (() => {
    const _el$ = _tmpl$$3();
    use(tooltip, _el$);
    spread(_el$, otherProps, false, true);
    insert(
      _el$,
      (() => {
        const _c$ = createMemo(() => typeof props.children === "function");
        return () => (_c$() ? props.children(childProps()) : props.children);
      })(),
    );
    return _el$;
  })();
};
/**
 * Poppers rely on the 3rd party library [Popper.js](https://popper.js.org/docs/v2/) for positioning.
 *
 * Demos:
 *
 * - [Popper](https://mui.com/components/popper/)
 *
 * API:
 *
 * - [PopperUnstyled API](https://mui.com/api/popper-unstyled/)
 */
const PopperUnstyled = $$j.component(function PopperUnstyled({
  otherProps,
  props,
}) {
  const [exited, setExited] = createSignal(true);
  const returnNull = createMemo(
    () => !props.keepMounted && !props.open && (!props.transition || exited()),
  );
  // If the container prop is provided, use that
  // If the anchorEl prop is provided, use its parent body element as the container
  // If neither are provided let the Modal take care of choosing the container
  const container = createMemo(
    () =>
      props.container ||
      (props.anchorEl
        ? ownerDocument(resolveAnchorEl(props.anchorEl)).body
        : undefined),
  );
  return createComponent(TransitionContext.Provider, {
    value: {
      get in() {
        return props.transition && props.open;
      },
      onEnter: () => {
        if (props.transition) setExited(false);
      },
      onExited: () => {
        if (props.transition) setExited(true);
      },
    },
    get children() {
      return createComponent(Show, {
        get when() {
          return !returnNull();
        },
        get children() {
          return createComponent(Portal, {
            get disablePortal() {
              return props.disablePortal;
            },
            get container() {
              return container();
            },
            get children() {
              return createComponent(
                PopperTooltip,
                mergeProps(
                  {
                    get anchorEl() {
                      return props.anchorEl;
                    },
                    get direction() {
                      return props.direction;
                    },
                    get disablePortal() {
                      return props.disablePortal;
                    },
                    get modifiers() {
                      return props.modifiers;
                    },
                    get open() {
                      return createMemo(() => !!props.transition)()
                        ? !exited()
                        : props.open;
                    },
                    get placement() {
                      return props.placement;
                    },
                    get popperOptions() {
                      return props.popperOptions;
                    },
                    get popperRef() {
                      return props.popperRef;
                    },
                  },
                  otherProps,
                  {
                    get style() {
                      return {
                        // Prevents scroll issue, waiting for Popper.js to add this style once initiated.
                        //position: "fixed", // [review] Don't required in SolidJS
                        // Fix Popper.js display issue
                        top: 0,
                        left: 0,
                        display:
                          !props.open &&
                          props.keepMounted &&
                          (!props.transition || exited())
                            ? "none"
                            : null,
                        ...(otherProps.style &&
                        typeof otherProps.style !== "string"
                          ? otherProps.style
                          : {}),
                      };
                    },
                    get children() {
                      return props.children;
                    },
                  },
                ),
              );
            },
          });
        },
      });
    },
  });
});

const $$i = createComponentFactory()({
  name: "MuiPopper",
  selfPropNames: [],
});

/**
 *
 * Demos:
 *
 * - [Autocomplete](https://mui.com/components/autocomplete/)
 * - [Menus](https://mui.com/components/menus/)
 * - [Popper](https://mui.com/components/popper/)
 *
 * API:
 *
 * - [Popper API](https://mui.com/api/popper/)
 */
const Popper = $$i.component(function Popper({ otherProps }) {
  const theme = useTheme$1();
  return createComponent(
    PopperUnstyled,
    mergeProps(
      {
        get direction() {
          return theme?.direction;
        },
      },
      otherProps,
    ),
  );
});

const RadioGroupContext = createContext(void 0);

function useRadioGroup() {
  return useContext(RadioGroupContext);
}

const _tmpl$$2 = /*#__PURE__*/ template(
  `<svg><path d="M8.465 8.465C9.37 7.56 10.62 7 12 7C14.76 7 17 9.24 17 12C17 13.38 16.44 14.63 15.535 15.535C14.63 16.44 13.38 17 12 17C9.24 17 7 14.76 7 12C7 10.62 7.56 9.37 8.465 8.465Z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const RadioButtonCheckedIcon = createSvgIcon(
  () => _tmpl$$2(),
  "RadioButtonChecked",
);

const _tmpl$$1 = /*#__PURE__*/ template(
  `<svg><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"></svg>`,
  false,
  true,
);

/**
 * @ignore - internal component.
 */
const RadioButtonUncheckedIcon = createSvgIcon(
  () => _tmpl$$1(),
  "RadioButtonUnchecked",
);

const RadioButtonIconRoot = styled$1("span")({
  position: "relative",
  display: "flex",
});
const RadioButtonIconBackground = styled$1(RadioButtonUncheckedIcon)({
  // Scale applied to prevent dot misalignment in Safari
  transform: "scale(1)",
});
const RadioButtonIconDot = styled$1(RadioButtonCheckedIcon)(
  ({ theme, ownerState }) => ({
    left: 0,
    position: "absolute",
    transform: "scale(0)",
    transition: theme.transitions.create("transform", {
      easing: theme.transitions.easing.easeIn,
      duration: theme.transitions.duration.shortest,
    }),
    ...(ownerState.checked && {
      transform: "scale(1)",
      transition: theme.transitions.create("transform", {
        easing: theme.transitions.easing.easeOut,
        duration: theme.transitions.duration.shortest,
      }),
    }),
  }),
);

/**
 * @ignore - internal component.
 */
function RadioButtonIcon(props) {
  const ownerState = mergeProps(
    {
      checked: false,
      classes: {},
    },
    props,
  );
  return createComponent(RadioButtonIconRoot, {
    get ["class"]() {
      return ownerState.classes.root;
    },
    ownerState: ownerState,
    get children() {
      return [
        createComponent(RadioButtonIconBackground, {
          get fontSize() {
            return props.fontSize;
          },
          get ["class"]() {
            return ownerState.classes.background;
          },
          ownerState: ownerState,
        }),
        createComponent(RadioButtonIconDot, {
          get fontSize() {
            return props.fontSize;
          },
          get ["class"]() {
            return ownerState.classes.dot;
          },
          ownerState: ownerState,
        }),
      ];
    },
  });
}

function getRadioUtilityClass(slot) {
  return generateUtilityClass("MuiRadio", slot);
}
const radioClasses = generateUtilityClasses("MuiRadio", [
  "root",
  "checked",
  "disabled",
  "colorPrimary",
  "colorSecondary",
]);

const $$h = createComponentFactory()({
  name: "MuiRadio",
  propDefaults: ({ set }) =>
    set({
      checkedIcon: defaultCheckedIcon,
      color: "primary",
      icon: defaultIcon,
      size: "medium",
    }),
  selfPropNames: [
    "checkedIcon",
    "classes",
    "color",
    "disabled",
    "icon",
    "size",
  ],
  utilityClass: getRadioUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", `color${capitalize(ownerState.color)}`],
  }),
});
const RadioRoot = styled$1(SwitchBase, {
  skipProps: skipRootProps.filter((v) => v !== "classes"),
  name: "MuiRadio",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [styles.root, styles[`color${capitalize(props.ownerState.color)}`]];
  },
})(({ theme, ownerState }) => ({
  color: theme.palette.text.secondary,
  "&:hover": {
    backgroundColor: alpha(
      ownerState.color === "default"
        ? theme.palette.action.active
        : theme.palette[ownerState.color].main,
      theme.palette.action.hoverOpacity,
    ),
    // Reset on touch devices, it doesn't add specificity
    "@media (hover: none)": {
      backgroundColor: "transparent",
    },
  },
  ...(ownerState.color !== "default" && {
    [`&.${radioClasses.checked}`]: {
      color: theme.palette[ownerState.color].main,
    },
  }),
  [`&.${radioClasses.disabled}`]: {
    color: theme.palette.action.disabled,
  },
}));
function areEqualValues$1(a, b) {
  if (typeof b === "object" && b !== null) {
    return a === b;
  }

  // The value could be a number, the DOM will stringify it anyway.
  return String(a) === String(b);
}
const defaultCheckedIcon = () =>
  createComponent(RadioButtonIcon, {
    checked: true,
  });
const defaultIcon = () => createComponent(RadioButtonIcon, {});
const Radio = $$h.component(function Radio({
  classes,
  allProps,
  otherProps,
  props,
}) {
  const radioGroup = useRadioGroup();
  const formControlLabelContext = useContext(FormControlLabelContext);
  const name = () =>
    otherProps.name ?? formControlLabelContext?.name ?? radioGroup?.name;
  const value = () => otherProps.value ?? formControlLabelContext?.value;
  const checked = () =>
    otherProps.checked ??
    formControlLabelContext?.checked ??
    areEqualValues$1(radioGroup?.value, value());
  const haveDefaultIcons = () =>
    props.checkedIcon === defaultCheckedIcon && props.icon === defaultIcon;
  return createComponent(SvgIconContext.Provider, {
    value: {
      get fontSize() {
        return props.size;
      },
    },
    get children() {
      return createComponent(
        RadioRoot,
        mergeProps(
          {
            type: "radio",
          },
          otherProps,
          {
            ownerState: allProps,
            classes: classes,
            get name() {
              return name();
            },
            get value() {
              return value();
            },
            get checked() {
              return checked();
            },
            onChange: (event, checked) => {
              otherProps.onChange?.(event, checked);
              formControlLabelContext?.onChange?.(event, checked);
              radioGroup?.onChange?.(event, checked);
            },
          },
          () =>
            haveDefaultIcons()
              ? {
                  get children() {
                    // Avoid recreating the component for CSS animations
                    return createComponent(RadioButtonIcon, {
                      get checked() {
                        return checked();
                      },
                    });
                  },
                  get icon() {
                    return [];
                  },
                  get checkedIcon() {
                    return [];
                  },
                }
              : {
                  get icon() {
                    return props.icon;
                  },
                  get checkedIcon() {
                    return props.checkedIcon;
                  },
                },
        ),
      );
    },
  });
});

const $$g = createComponentFactory()({
  name: "MuiRadioGroup",
  selfPropNames: ["defaultValue", "name", "onChange", "value"],
});
const RadioGroup = $$g.component(function RadioGroup({ props, otherProps }) {
  const element = createRef();
  const [value, setValueState] = useControlled({
    controlled: () => props.value,
    default: () => props.defaultValue,
    name: "RadioGroup",
  });
  const name = createUniqueId(() => props.name);
  return createComponent(RadioGroupContext.Provider, {
    value: {
      get name() {
        return name();
      },
      onChange(event) {
        setValueState(event.target.value);
        if (props.onChange) {
          props.onChange(event, event.target.value);
        }
      },
      get value() {
        return value();
      },
    },
    get children() {
      return createComponent(
        FormGroup,
        mergeProps(
          {
            role: "radiogroup",
            ref: element,
          },
          otherProps,
          {
            get children() {
              return otherProps.children;
            },
          },
        ),
      );
    },
  });
});

function getSelectUtilityClasses(slot) {
  return generateUtilityClass("MuiSelect", slot);
}
const selectClasses = generateUtilityClasses("MuiSelect", [
  "select",
  "multiple",
  "filled",
  "outlined",
  "standard",
  "disabled",
  "focused",
  "icon",
  "iconOpen",
  "iconFilled",
  "iconOutlined",
  "iconStandard",
  "nativeInput",
]);

const _tmpl$ = /*#__PURE__*/ template(`<span class="notranslate">&#8203;`);
const $$f = createComponentFactory()({
  name: "MuiSelectInput",
  selfPropNames: [
    "autoFocus",
    "autoWidth",
    "defaultOpen",
    "disabled",
    "IconComponent",
    "inputRef",
    "MenuProps",
    "multiple",
    "name",
    "native",
    "onBlur",
    "onChange",
    "onClose",
    "onFocus",
    "onOpen",
    "open",
    "readOnly",
    "renderValue",
    "SelectDisplayProps",
    "tabIndex",
    "value",
    "variant",
  ],
  utilityClass: getSelectUtilityClasses,
  slotClasses: (ownerState) => ({
    select: [
      "select",
      ownerState.variant,
      ownerState.disabled && "disabled",
      ownerState.multiple && "multiple",
    ],
    icon: [
      "icon",
      `icon${capitalize(ownerState.variant)}`,
      ownerState.open && "iconOpen",
      ownerState.disabled && "disabled",
    ],
    nativeInput: ["nativeInput"],
  }),
});
const SelectSelect = styled$1("div", {
  name: "MuiSelect",
  slot: "Select",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      // Win specificity over the input base
      {
        [`&.${selectClasses.select}`]: styles.select,
      },
      {
        [`&.${selectClasses.select}`]: styles[ownerState.variant],
      },
      {
        [`&.${selectClasses.multiple}`]: styles.multiple,
      },
    ];
  },
})(nativeSelectSelectStyles, {
  // Win specificity over the input base
  [`&.${selectClasses.select}`]: {
    height: "auto",
    // Resets for multiple select with chips
    minHeight: "1.4375em",
    // Required for select\text-field height consistency
    textOverflow: "ellipsis",
    whiteSpace: "nowrap",
    overflow: "hidden",
  },
});
const SelectIcon = styled$1("svg", {
  name: "MuiSelect",
  slot: "Icon",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.icon,
      ownerState.variant && styles[`icon${capitalize(ownerState.variant)}`],
      ownerState.open && styles.iconOpen,
    ];
  },
})(nativeSelectIconStyles);
const SelectNativeInput = styled$1("input", {
  skipProps: skipRootProps,
  name: "MuiSelect",
  slot: "NativeInput",
  overridesResolver: (props, styles) => styles.nativeInput,
})({
  bottom: 0,
  left: 0,
  position: "absolute",
  opacity: 0,
  pointerEvents: "none",
  width: "100%",
  boxSizing: "border-box",
});
function areEqualValues(a, b) {
  if (typeof b === "object" && b !== null) {
    return a === b;
  }

  // The value could be a number, the DOM will stringify it anyway.
  return String(a) === String(b);
}
function isEmpty(display) {
  return display == null || (typeof display === "string" && !display.trim());
}

/**
 * @ignore - internal component.
 */
const SelectInput = $$f.defineComponent(function SelectInput(props) {
  const ref = createRef(props);
  const [, other] = splitProps(props, [
    "ref",
    "sx",
    // [new]
    "aria-describedby",
    "aria-label",
    "autoFocus",
    "autoWidth",
    "children",
    "class",
    "defaultOpen",
    "defaultValue",
    "disabled",
    "displayEmpty",
    "IconComponent",
    "inputRef",
    "labelId",
    "MenuProps",
    "multiple",
    "name",
    "onBlur",
    "onChange",
    "onClose",
    "onFocus",
    "onOpen",
    "open",
    "readOnly",
    "renderValue",
    "SelectDisplayProps",
    "tabIndex",
    "type",
    "value",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      MenuProps: {},
      SelectDisplayProps: {},
      variant: "standard",
    },
    props,
  );
  const [value, setValueState] = useControlled({
    controlled: () => props.value,
    default: () => props.defaultValue,
    name: "Select",
  });
  const [openState, setOpenState] = useControlled({
    controlled: () => props.open,
    default: () => props.defaultOpen,
    name: "Select",
  });
  const inputRef = createRef();
  const displayRef = createRef();
  const [displayNode, setDisplayNode] = createSignal(null);
  const isOpenControlled = props.open != null;
  const [menuMinWidthState, setMenuMinWidthState] = createSignal(null);
  const handleDisplayRef = (node) => {
    displayRef.current = node;
    setDisplayNode(node);
  };
  const action = {
    nodeName: "INPUT",
    get node() {
      return inputRef.current;
    },
    addEventListener(eventName, callback) {
      if (eventName !== "input")
        throw new Error(`Invalid event name: ${eventName}`);
      inputRef.current.addEventListener(eventName, callback);
    },
    get value() {
      return value();
    },
    set value(v) {
      setValueState(v);
    },
    focus() {
      inputRef.current.focus();
    },
  };

  // Resize menu on `defaultOpen` automatic toggle.
  createEffect(
    on(
      () => [displayNode(), props.autoWidth],
      () => {
        if (
          props.defaultOpen &&
          openState() &&
          displayNode() &&
          !isOpenControlled
        ) {
          setMenuMinWidthState(
            props.autoWidth ? null : displayNode().clientWidth,
          );
          displayRef.current.focus();
        }
      },
    ),
  );
  // `isOpenControlled` is ignored because the component should never switch between controlled and uncontrolled modes.
  // `defaultOpen` and `openState` are ignored to avoid unnecessary callbacks.
  createEffect(() => {
    if (props.autoFocus) {
      displayRef.current.focus();
    }
  });
  let clickHandler;
  createEffect(() => {
    if (!props.labelId) {
      return;
    }
    const label = ownerDocument(displayRef.current).getElementById(
      props.labelId,
    );
    if (label) {
      if (clickHandler) label.removeEventListener("click", clickHandler);
      clickHandler = () => {
        if (getSelection().isCollapsed) {
          displayRef.current.focus();
        }
      };
      label.addEventListener("click", clickHandler);
    }
  });
  const update = (open, event) => {
    // [review] FocusTrap
    if (!open) displayRef.current.focus();
    if (open) {
      if (props.onOpen) {
        props.onOpen(event);
      }
    } else if (props.onClose) {
      props.onClose(event);
    }
    if (!isOpenControlled) {
      setMenuMinWidthState(props.autoWidth ? null : displayNode().clientWidth);
      setOpenState(open);
    }
  };
  const handleMouseDown = (event) => {
    if (props.disabled || props.readOnly) return;
    // Ignore everything but left-click
    if (event.button !== 0) {
      return;
    }
    // Hijack the default focus behavior.
    event.preventDefault();
    displayRef.current.focus();
    update(true, event);
  };
  const handleClose = (event) => {
    update(false, event);
  };
  const childrenArray = inspectChildren(() => props.children);
  const selected = (child) => {
    const v = value();
    if (props.multiple) {
      if (!Array.isArray(v)) {
        throw new Error(
          "MUI: The `value` prop must be an array " +
            "when using the `Select` component with `multiple`.",
        );
      }
      return v.some((v) => areEqualValues(v, child.props.value));
    } else {
      return areEqualValues(v, child.props.value);
    }
  };
  const items = mapArray(childrenArray, (child) => {
    if (isComponentObject(child, MenuItem)) {
      setMenuItemProps((prev) => [...prev, child.props]);
      onCleanup(() =>
        setMenuItemProps((prev) => prev.filter((v) => v !== child.props)),
      );
      const isSelected = createMemo(() => selected(child));
      const childProps = mergeProps(child.props, {
        get "aria-selected"() {
          return isSelected();
        },
        onClick: (event) => handleItemClick(event, childProps, child.props),
        onKeyUp: (event) => {
          if (event.key === " ") {
            // otherwise our MenuItems dispatches a click event
            // it's not behavior of the native <option> and causes
            // the select to close immediately since we open on space keydown
            event.preventDefault();
          }
          if (typeof child.props.onKeyUp === "function") {
            child.props.onKeyUp(event);
          }
        },
        role: "option",
        get selected() {
          return isSelected();
        },
        value: undefined,
        // The value is most likely not a valid HTML attribute.
        "data-value": child.props.value, // Instead, we provide it as a data attribute.
      });

      return createComponent(child.Component, childProps);
    } else {
      return isComponentObject(child)
        ? createComponent(child.Component, child.props)
        : child;
    }
  });

  // [pending] Support autofill.
  /*const handleChange = (event: SelectChangeEvent<any>) => {
    const items = childrenArray();
    const index = items
      .map((child) => child.props.value)
      .indexOf(event.target.value);
     if (index === -1) {
      return;
    }
     const child = items[index];
    setValueState(child.props.value);
    if (props.onChange) {
      props.onChange(event, event.target);
    }
  };*/

  const handleItemClick = (event, itemProps, prevProps) => {
    let newValue;
    const child = {
      props: itemProps,
    };
    const multiple = props.multiple;
    // We use the tabindex attribute to signal the available options.
    if (!event.currentTarget.hasAttribute("tabindex")) {
      return;
    }
    if (multiple) {
      newValue = Array.isArray(value()) ? value().slice() : [];
      const itemIndex = value().indexOf(child.props.value);
      if (itemIndex === -1) {
        newValue.push(child.props.value);
      } else {
        newValue.splice(itemIndex, 1);
      }
    } else {
      newValue = child.props.value;
    }
    if (typeof prevProps.onClick === "function") {
      prevProps.onClick(event);
    }
    if (value() !== newValue) {
      const inputEvent = new CustomEvent("input");
      Object.defineProperty(inputEvent, "target", {
        writable: false,
        value: {
          name: itemProps.name,
          get value() {
            return newValue;
          },
          set value(v) {
            action.value = v;
          },
        },
      });
      inputRef.current.dispatchEvent(inputEvent);
      setValueState(newValue);
      if (typeof itemProps.onChange === "function") {
        itemProps.onChange(inputEvent, inputEvent.target);
      }
    }
    if (!multiple) {
      update(false, event);
    }
  };
  const handleKeyDown = (event) => {
    if (!props.readOnly) {
      const validKeys = [
        " ",
        "ArrowUp",
        "ArrowDown",
        // The native select doesn't respond to enter on MacOS, but it's recommended by
        // https://www.w3.org/TR/wai-aria-practices/examples/listbox/listbox-collapsible.html
        "Enter",
      ];
      if (validKeys.indexOf(event.key) !== -1) {
        event.preventDefault();
        update(true, event);
      }
    }
  };
  const open = () => displayNode() !== null && !!openState();
  const handleBlur = (event) => {
    // if open event.stopImmediatePropagation
    if (!open() && props.onBlur) {
      // Preact support, target is read only property on a native event.
      Object.defineProperty(event, "target", {
        writable: true,
        value: {
          value: value(),
          name: props.name,
        },
      });
      props.onBlur(event);
    }
  };
  const [menuItemProps, setMenuItemProps] = createStore([]);
  const display = () => {
    const v = value();
    if (
      isFilled({
        value: v,
      }) ||
      props.displayEmpty
    ) {
      if (props.renderValue) {
        return props.renderValue(v);
      } else if (props.multiple) {
        const result = [];
        for (const itemProps of menuItemProps) {
          if (!Array.isArray(v)) {
            throw new Error(
              "MUI: The `value` prop must be an array " +
                "when using the `Select` component with `multiple`.",
            );
          }
          if (v.some((v) => areEqualValues(v, itemProps.value)))
            result.push(itemProps.children);
        }
        return result.reduce((output, child, index) => {
          output.push(child);
          if (index < result.length - 1) {
            output.push(", ");
          }
          return output;
        }, []);
      } else {
        for (const itemProps of menuItemProps) {
          if (areEqualValues(v, itemProps.value)) {
            return itemProps.children;
          }
        }
      }
    }
  };

  /*if ("production" !== "production") {
    createEffect(
      on(
        () => [foundMatch, items(), props.multiple, props.name, value()],
        () => {
          if (!foundMatch && !props.multiple && value() !== "") {
            const values = items().filter((child => isComponentObject(child, MenuItem)).map((child) => child.props.value);
            console.warn(
              [
                `MUI: You have provided an out-of-range value \`${value}\` for the select ${
                  props.name ? `(name="${props.name}") ` : ""
                }component.`,
                "Consider providing a value that matches one of the available options or ''.",
                `The available values are ${
                  values
                    .filter((x) => x != null)
                    .map((x) => `\`${x}\``)
                    .join(", ") || '""'
                }.`,
              ].join("\n")
            );
          }
        }
      )
    );
  }*/

  // Avoid performing a layout computation in the render method.
  const menuMinWidth = () => {
    let menuMinWidth = menuMinWidthState();
    if (!props.autoWidth && isOpenControlled && displayNode()) {
      menuMinWidth = displayNode().clientWidth;
    }
    return menuMinWidth;
  };
  const PaperProps = mergeProps(() => baseProps.MenuProps.PaperProps, {
    style: mergeProps(
      {
        get "min-width"() {
          const v = menuMinWidth();
          return typeof v === "number" ? `${v}px` : undefined;
        },
      },
      () =>
        baseProps.MenuProps.PaperProps != null
          ? baseProps.MenuProps.PaperProps.style
          : null,
    ),
  });
  const MenuListProps = mergeProps(
    {
      get "aria-labelledby"() {
        return props.labelId;
      },
      ["role"]: "listbox",
      disableListWrap: true,
    },
    () => baseProps.MenuProps.MenuListProps,
  );
  const tabIndex = () => {
    if (typeof props.tabIndex !== "undefined") {
      return props.tabIndex;
    } else {
      return props.disabled ? undefined : 0;
    }
  };
  const buttonId = () =>
    baseProps.SelectDisplayProps.id ||
    (props.name ? `mui-component-select-${props.name}` : undefined);
  const ownerState = mergeProps(props, {
    get variant() {
      return baseProps.variant;
    },
    get value() {
      return value();
    },
    get open() {
      return open();
    },
  });
  const classes = $$f.useClasses(ownerState);
  const nativeSelectValue = () => {
    const v = value();
    return Array.isArray(v) ? v.join(",") : v;
  };
  const displayValue = () => {
    const v = display();
    /* So the vertical align positioning algorithm kicks in.*/
    return isEmpty(v) // notranslate needed while Google Translate will not fix zero-width space issue
      ? _tmpl$()
      : v;
  };
  return [
    createComponent(
      SelectSelect,
      mergeProps(
        {
          ref: handleDisplayRef,
          get tabIndex() {
            return tabIndex();
          },
          role: "button",
          get ["aria-disabled"]() {
            return props.disabled ? "true" : undefined;
          },
          get ["aria-expanded"]() {
            return open() ? "true" : "false";
          },
          "aria-haspopup": "listbox",
          get ["aria-label"]() {
            return props["aria-label"];
          },
          get ["aria-labelledby"]() {
            return (
              [props.labelId, buttonId()].filter(Boolean).join(" ") || undefined
            );
          },
          get ["aria-describedby"]() {
            return props["aria-describedby"];
          },
          onKeyDown: handleKeyDown,
          onMouseDown: handleMouseDown,
          onBlur: handleBlur,
          get onFocus() {
            return props.onFocus;
          },
        },
        () => baseProps.SelectDisplayProps,
        {
          ownerState: ownerState,
          get sx() {
            return props.sx;
          },
          get ["class"]() {
            return clsx(
              classes.select,
              props.class,
              baseProps.SelectDisplayProps.class,
            );
          },
          get id() {
            return buttonId();
          },
          get children() {
            return displayValue();
          },
        },
      ),
    ),
    createComponent(
      SelectNativeInput,
      mergeProps(
        {
          get value() {
            return nativeSelectValue();
          },
          get ["data-value"]() {
            return nativeSelectValue();
          },
          get name() {
            return props.name;
          },
          "aria-hidden": true,
          ref: (e) => {
            inputRef(e);
            if (typeof props.inputRef === "function") props.inputRef(action);
            ref(action);
          },
          get onChange() {
            return props.onChange /* handleChange */;
          },
          tabIndex: -1,
          get disabled() {
            return props.disabled;
          },
          get ["class"]() {
            return classes.nativeInput;
          },
          get autofocus() {
            return props.autoFocus;
          },
          ownerState: ownerState,
        },
        other,
      ),
    ),
    createComponent(SelectIcon, {
      get as() {
        return props.IconComponent;
      },
      get ["class"]() {
        return classes.icon;
      },
      ownerState: ownerState,
    }),
    createComponent(
      Menu,
      mergeProps(
        {
          get id() {
            return `menu-${props.name || ""}`;
          },
          get anchorEl() {
            return displayNode();
          },
          get open() {
            return open();
          },
          onClose: handleClose,
          anchorOrigin: {
            vertical: "bottom",
            horizontal: "center",
          },
          transformOrigin: {
            vertical: "top",
            horizontal: "center",
          },
        },
        () => baseProps.MenuProps,
        {
          MenuListProps: MenuListProps,
          PaperProps: PaperProps,
          get children() {
            return items();
          },
        },
      ),
    ),
  ];
});

const $$e = createComponentFactory()({
  name: "MuiSelect",
  selfPropNames: [
    "autoWidth",
    "children",
    "classes",
    "defaultOpen",
    "defaultValue",
    "displayEmpty",
    "IconComponent",
    "id",
    "input",
    "inputProps",
    "label",
    "labelId",
    "MenuProps",
    "multiple",
    "native",
    "onChange",
    "onClose",
    "onOpen",
    "open",
    "renderValue",
    "SelectDisplayProps",
    "value",
    "variant",
  ],
});
const styledRootConfig = {
  name: "MuiSelect",
  overridesResolver: (props, styles) => styles.root,
  skipProps: [...skipRootProps, "variant"],
  slot: "Root",
};
const StyledInput = styled$1(Input, styledRootConfig)({});
const StyledOutlinedInput = styled$1(OutlinedInput, styledRootConfig)({});
const StyledFilledInput = styled$1(FilledInput, styledRootConfig)({});

/**
 *
 * Demos:
 *
 * - [Selects](https://mui.com/components/selects/)
 *
 * API:
 *
 * - [Select API](https://mui.com/api/select/)
 * - inherits [OutlinedInput API](https://mui.com/api/outlined-input/)
 */
const Select = $$e.defineComponent(function Select(inProps) {
  const props = $$e.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, [
    "autoWidth",
    "children",
    "classes",
    "class",
    "defaultOpen",
    "displayEmpty",
    "IconComponent",
    "id",
    "input",
    "inputProps",
    "label",
    "labelId",
    "MenuProps",
    "multiple",
    "native",
    "onClose",
    "onOpen",
    "open",
    "renderValue",
    "SelectDisplayProps",
    "variant",
  ]);
  const baseProps = mergeProps(
    {
      autoWidth: false,
      classes: {},
      defaultOpen: false,
      displayEmpty: false,
      IconComponent: ArrowDropDownIcon,
      multiple: false,
      native: false,
      variant: "outlined",
    },
    props,
  );
  const inputComponent = () =>
    baseProps.native ? NativeSelectInput : SelectInput;
  const muiFormControl = useFormControl();
  const fcs = formControlState({
    props: props,
    muiFormControl: muiFormControl,
    states: ["variant"],
  });
  const variant = () => fcs.variant || baseProps.variant;
  const inputElements = inspectChildren(() => props.input);
  const inputElement = () => {
    const [item] = inputElements();
    if (item) {
      if (isComponentObject(item)) return item;
      throw new Error(`Invalid component`);
    }
  };
  const InputComponent = () =>
    inputElement()?.Component ||
    {
      standard: StyledInput,
      outlined: StyledOutlinedInput,
      filled: StyledFilledInput,
    }[variant()];
  const ownerState = mergeProps(props, {
    get variant() {
      return variant();
    },
    get classes() {
      return baseProps.classes;
    },
  });
  const classes = $$e.useClasses(ownerState);
  const inputProps = mergeProps(
    {
      get children() {
        return props.children;
      },
      get IconComponent() {
        return baseProps.IconComponent;
      },
      get variant() {
        return variant();
      },
      type: undefined,
      get multiple() {
        return baseProps.multiple;
      },
    },
    () =>
      baseProps.native
        ? {
            id: props.id,
          }
        : {
            autoWidth: baseProps.autoWidth,
            defaultOpen: baseProps.defaultOpen,
            displayEmpty: baseProps.displayEmpty,
            labelId: props.labelId,
            MenuProps: props.MenuProps,
            onClose: props.onClose,
            onOpen: props.onOpen,
            open: props.open,
            renderValue: props.renderValue,
            SelectDisplayProps: {
              id: props.id,
              ...props.SelectDisplayProps,
            },
          },
    () => props.inputProps,
    {
      get classes() {
        return props.inputProps
          ? deepmerge(classes, props.inputProps.classes)
          : classes;
      },
    },
    () => inputElement()?.props?.inputProps,
  );
  return createComponent(
    Dynamic,
    mergeProps(
      {
        get $component() {
          return InputComponent();
        },
      },
      () => inputElement()?.props,
      {
        get inputComponent() {
          return inputComponent();
        },
        inputProps: inputProps,
        get notched() {
          return props.multiple && props.native && variant() === "outlined"
            ? true
            : undefined;
        },
        get label() {
          return !inputElement() && variant() === "outlined"
            ? props.label
            : undefined;
        },
        get ["class"]() {
          return props.class;
        },
        get variant() {
          return variant();
        },
      },
      other,
    ),
  );
});

function getUnit(input) {
  return String(input).match(/[\d.\-+]*\s*(.*)/)?.[1] || "";
}
function toUnitless(length) {
  return parseFloat(length.toString());
}

function getSkeletonUtilityClass(slot) {
  return generateUtilityClass("suidSkeleton", slot);
}
generateUtilityClasses("suidSkeleton", [
  "root",
  "text",
  "rectangular",
  "circular",
  "pulse",
  "wave",
  "withChildren",
  "fitContent",
  "heightAuto",
]);

const $$d = createComponentFactory()({
  name: "MuiSkeleton",
  selfPropNames: [
    "animation",
    "children",
    "classes",
    "height",
    "variant",
    "width",
  ],
  propDefaults: ({ set }) =>
    set({
      animation: "pulse",
      component: "span",
      variant: "text",
    }),
  utilityClass: getSkeletonUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      ownerState.animation,
      ownerState.hasChildren && "withChildren",
      ownerState.hasChildren && !ownerState.width && "fitContent",
      ownerState.hasChildren && !ownerState.height && "heightAuto",
    ],
  }),
});
const SkeletonRoot = styled$1("span", {
  name: "MuiSkeleton",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      styles[ownerState.variant],
      ownerState.animation !== false && styles[ownerState.animation],
      ownerState.hasChildren && styles.withChildren,
      ownerState.hasChildren && !ownerState.width && styles.fitContent,
      ownerState.hasChildren && !ownerState.height && styles.heightAuto,
    ];
  },
})(
  ({ theme, ownerState }) => {
    const radiusUnit = getUnit(theme.shape.borderRadius) || "px";
    const radiusValue = toUnitless(theme.shape.borderRadius);
    return {
      display: "block",
      // Create a "on paper" color with sufficient contrast retaining the color
      backgroundColor: alpha(
        theme.palette.text.primary,
        theme.palette.mode === "light" ? 0.11 : 0.13,
      ),
      height: "1.2em",
      ...(ownerState.variant === "text" && {
        marginTop: 0,
        marginBottom: 0,
        height: "auto",
        transformOrigin: "0 55%",
        transform: "scale(1, 0.60)",
        borderRadius: `${radiusValue}${radiusUnit}/${
          Math.round((radiusValue / 0.6) * 10) / 10
        }${radiusUnit}`,
        "&:empty:before": {
          content: '"\\00a0"',
        },
      }),
      ...(ownerState.variant === "circular" && {
        borderRadius: "50%",
      }),
      ...(ownerState.hasChildren && {
        "& > *": {
          visibility: "hidden",
        },
      }),
      ...(ownerState.hasChildren &&
        !ownerState.width && {
          maxWidth: "fit-content",
        }),
      ...(ownerState.hasChildren &&
        !ownerState.height && {
          height: "auto",
        }),
    };
  },
  ({ ownerState }) =>
    ownerState.animation === "pulse" && {
      "@keyframes pulse-animation-$id": {
        0: {
          opacity: 1,
        },
        50: {
          opacity: 0.4,
        },
        100: {
          opacity: 1,
        },
      },
      animation: "pulse-animation-$id 1.5s ease-in-out 0.5s infinite",
    },
  ({ ownerState, theme }) =>
    ownerState.animation === "wave" && {
      position: "relative",
      overflow: "hidden",
      "@keyframes wave-animation-$id": {
        0: {
          transform: "translateX(-100%)",
        },
        50: {
          // +0.5s of delay between each loop
          transform: "translateX(100%)",
        },
        100: {
          transform: "translateX(100%)",
        },
      },
      // Fix bug in Safari https://bugs.webkit.org/show_bug.cgi?id=68196
      "-webkit-mask-image": "-webkit-radial-gradient(white, black);",
      "&::after": {
        animation: "wave-animation-$id 1.6s linear 0.5s infinite",
        background: `linear-gradient(90deg, transparent, ${theme.palette.action.hover}, transparent)`,
        content: '""',
        position: "absolute",
        // Avoid flash during server-side hydration
        transform: "translateX(-100%)",
        bottom: 0,
        left: 0,
        right: 0,
        top: 0,
      },
    },
);

/**
 *
 * Demos:
 *
 * - [Skeleton](https://mui.com/components/skeleton/)
 *
 * API:
 *
 * - [Skeleton API](https://mui.com/api/skeleton/)
 */

const Skeleton = $$d.component(function Skeleton({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const hasChildren = () =>
    typeof props.children === "undefined"
      ? false
      : Array.isArray(props.children)
      ? !!props.children.length
      : true;
  const ownerState = mergeProps(allProps, {
    get hasChildren() {
      return hasChildren();
    },
  });
  return createComponent(
    SkeletonRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: ownerState,
      get sx() {
        return {
          width: props.width,
          height: props.height,
          ...(otherProps.sx || {}),
        };
      },
    }),
  );
});

function mergeSxObjects(object, ...sources) {
  return sources.reduce(
    (target, source, index) =>
      deepmerge(target, source, {
        clone: !!index,
        sortKeys: true,
      }),
    object,
  );
}

const $$c = createComponentFactory()({
  name: "MuiStack",
  selfPropNames: ["children", "direction", "divider", "spacing"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
      direction: "column",
      spacing: 0,
    }),
});
function joinChildren(children, separator) {
  const childrenArray = (
    Array.isArray(children) ? children : [children]
  ).filter(Boolean);
  return childrenArray.reduce((output, child, index) => {
    output.push(child);
    if (index < childrenArray.length - 1) {
      output.push(separator());
    }
    return output;
  }, []);
}
const getSideFromDirection = (direction) => {
  return {
    row: "Left",
    "row-reverse": "Right",
    column: "Top",
    "column-reverse": "Bottom",
  }[direction];
};
const StackRoot = styled$1("div", {
  name: "MuiStack",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [styles.root];
  },
})(({ theme, ownerState }) => {
  let styles = {
    display: "flex",
    ...handleBreakpoints(
      {
        theme,
      },
      resolveBreakpointValues({
        values: ownerState.direction,
        breakpoints: theme.breakpoints.values,
      }),
      (propValue) => ({
        flexDirection: propValue,
      }),
    ),
  };
  if (ownerState.spacing) {
    const transformer = createUnarySpacing(theme);
    const base = theme.breakpoints.keys.reduce((acc, breakpoint) => {
      if (
        ownerState.spacing[breakpoint] != null ||
        ownerState.direction[breakpoint] != null
      ) {
        acc[breakpoint] = true;
      }
      return acc;
    }, {});
    const directionValues = resolveBreakpointValues({
      values: ownerState.direction,
      base,
    });
    const spacingValues = resolveBreakpointValues({
      values: ownerState.spacing,
      base,
    });
    const styleFromPropValue = (propValue, breakpoint) => {
      return {
        "& > :not(style) + :not(style)": {
          margin: 0,
          [`margin${getSideFromDirection(
            breakpoint ? directionValues[breakpoint] : ownerState.direction,
          )}`]: transformer(propValue),
        },
      };
    };
    styles = mergeSxObjects(
      styles,
      handleBreakpoints(
        {
          theme,
        },
        spacingValues,
        styleFromPropValue,
      ),
    );
  }
  return styles;
});

/**
 *
 * Demos:
 *
 * - [Stack](https://mui.com/components/stack/)
 *
 * API:
 *
 * - [Stack API](https://mui.com/api/stack/)
 */
const Stack = $$c.component(function Stack({ allProps, otherProps, props }) {
  otherProps = extendSxProp(otherProps);
  return createComponent(
    StackRoot,
    mergeProps(
      {
        get as() {
          return otherProps.component;
        },
        ownerState: allProps,
      },
      otherProps,
      {
        get children() {
          return createComponent(Show, {
            get when() {
              return props.divider;
            },
            get fallback() {
              return props.children;
            },
            get children() {
              return joinChildren(props.children, () => props.divider);
            },
          });
        },
      },
    ),
  );
});

function getSwitchUtilityClass(slot) {
  return generateUtilityClass("MuiSwitch", slot);
}
const switchClasses = generateUtilityClasses("MuiSwitch", [
  "root",
  "edgeStart",
  "edgeEnd",
  "switchBase",
  "colorPrimary",
  "colorSecondary",
  "sizeSmall",
  "sizeMedium",
  "checked",
  "disabled",
  "input",
  "thumb",
  "track",
]);

const $$b = createComponentFactory()({
  name: "MuiSwitch",
  selfPropNames: [
    "checkedIcon",
    "classes",
    "color",
    "disabled",
    "icon",
    "size",
    "value",
  ],
  propDefaults: ({ set }) =>
    set({
      color: "primary",
      size: "medium",
    }),
  utilityClass: getSwitchUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !!ownerState.edge && `edge${capitalize(ownerState.edge)}`,
      `size${capitalize(ownerState.size)}`,
    ],
    switchBase: [
      "switchBase",
      `color${capitalize(ownerState.color)}`,
      !!ownerState.checked && "checked",
      !!ownerState.disabled && "disabled",
    ],
    thumb: ["thumb"],
    track: ["track"],
    input: ["input"],
  }),
});
const SwitchRoot = styled$1("span", {
  name: "MuiSwitch",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      ownerState.edge && styles[`edge${capitalize(ownerState.edge)}`],
      styles[`size${capitalize(ownerState.size)}`],
    ];
  },
})(({ ownerState }) => ({
  display: "inline-flex",
  width: 34 + 12 * 2,
  height: 14 + 12 * 2,
  overflow: "hidden",
  padding: 12,
  boxSizing: "border-box",
  position: "relative",
  flexShrink: 0,
  zIndex: 0,
  // Reset the stacking context.
  verticalAlign: "middle",
  // For correct alignment with the text.
  "@media print": {
    colorAdjust: "exact",
  },
  ...(ownerState.edge === "start" && {
    marginLeft: -8,
  }),
  ...(ownerState.edge === "end" && {
    marginRight: -8,
  }),
  ...(ownerState.size === "small" && {
    width: 40,
    height: 24,
    padding: 7,
    [`& .${switchClasses.thumb}`]: {
      width: 16,
      height: 16,
    },
    [`& .${switchClasses.switchBase}`]: {
      padding: 4,
      [`&.${switchClasses.checked}`]: {
        transform: "translateX(16px)",
      },
    },
  }),
}));
const SwitchSwitchBase = styled$1(SwitchBase, {
  name: "MuiSwitch",
  slot: "SwitchBase",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.switchBase,
      {
        [`& .${switchClasses.input}`]: styles.input,
      },
      ownerState.color !== "default" &&
        styles[`color${capitalize(ownerState.color)}`],
    ];
  },
})(
  ({ theme }) => ({
    position: "absolute",
    top: 0,
    left: 0,
    zIndex: 1,
    // Render above the focus ripple.
    color:
      theme.palette.mode === "light"
        ? theme.palette.common.white
        : theme.palette.grey[300],
    transition: theme.transitions.create(["left", "transform"], {
      duration: theme.transitions.duration.shortest,
    }),
    [`&.${switchClasses.checked}`]: {
      transform: "translateX(20px)",
    },
    [`&.${switchClasses.disabled}`]: {
      color:
        theme.palette.mode === "light"
          ? theme.palette.grey[100]
          : theme.palette.grey[600],
    },
    [`&.${switchClasses.checked} + .${switchClasses.track}`]: {
      opacity: 0.5,
    },
    [`&.${switchClasses.disabled} + .${switchClasses.track}`]: {
      opacity: theme.palette.mode === "light" ? 0.12 : 0.2,
    },
    [`& .${switchClasses.input}`]: {
      left: "-100%",
      width: "300%",
    },
  }),
  ({ theme, ownerState }) => ({
    "&:hover": {
      backgroundColor: alpha(
        theme.palette.action.active,
        theme.palette.action.hoverOpacity,
      ),
      // Reset on touch devices, it doesn't add specificity
      "@media (hover: none)": {
        backgroundColor: "transparent",
      },
    },
    ...(ownerState.color !== "default" && {
      [`&.${switchClasses.checked}`]: {
        color: theme.palette[ownerState.color].main,
        "&:hover": {
          backgroundColor: alpha(
            theme.palette[ownerState.color].main,
            theme.palette.action.hoverOpacity,
          ),
          "@media (hover: none)": {
            backgroundColor: "transparent",
          },
        },
        [`&.${switchClasses.disabled}`]: {
          color:
            theme.palette.mode === "light"
              ? lighten(theme.palette[ownerState.color].main, 0.62)
              : darken(theme.palette[ownerState.color].main, 0.55),
        },
      },
      [`&.${switchClasses.checked} + .${switchClasses.track}`]: {
        backgroundColor: theme.palette[ownerState.color].main,
      },
    }),
  }),
);
const SwitchTrack = styled$1("span", {
  name: "MuiSwitch",
  slot: "Track",
  overridesResolver: (props, styles) => styles.track,
})(({ theme }) => ({
  height: "100%",
  width: "100%",
  borderRadius: 14 / 2,
  zIndex: -1,
  transition: theme.transitions.create(["opacity", "background-color"], {
    duration: theme.transitions.duration.shortest,
  }),
  backgroundColor:
    theme.palette.mode === "light"
      ? theme.palette.common.black
      : theme.palette.common.white,
  opacity: theme.palette.mode === "light" ? 0.38 : 0.3,
}));
const SwitchThumb = styled$1("span", {
  name: "MuiSwitch",
  slot: "Thumb",
  overridesResolver: (props, styles) => styles.thumb,
})(({ theme }) => ({
  boxShadow: theme.shadows[1],
  backgroundColor: "currentColor",
  width: 20,
  height: 20,
  borderRadius: "50%",
}));

/**
 *
 * Demos:
 *
 * - [Switches](https://mui.com/components/switches/)
 * - [Transfer List](https://mui.com/components/transfer-list/)
 *
 * API:
 *
 * - [Switch API](https://mui.com/api/switch/)
 * - inherits [IconButton API](https://mui.com/api/icon-button/)
 */
const Switch = $$b.component(function Switch({
  allProps,
  props,
  classes,
  otherProps,
}) {
  const icon = () =>
    createComponent(SwitchThumb, {
      get ["class"]() {
        return classes.thumb;
      },
      ownerState: allProps,
    });
  const allClasses = mergeProps(classes, {
    get root() {
      return classes.switchBase;
    },
  });
  const [, otherPropsWithoutSx] = splitProps(otherProps, ["sx"]);
  const baseProps = mergeProps(otherPropsWithoutSx, {
    get checkedIcon() {
      return props.checkedIcon || icon;
    },
    get disabled() {
      return props.disabled;
    },
    get icon() {
      return props.icon || icon;
    },
    get value() {
      return props.value;
    },
  });
  return createComponent(SwitchRoot, {
    get ["class"]() {
      return clsx(classes.root, otherProps.class);
    },
    get sx() {
      return otherProps.sx;
    },
    ownerState: allProps,
    get children() {
      return [
        createComponent(
          SwitchSwitchBase,
          mergeProps(
            {
              type: "checkbox",
              ownerState: allProps,
            },
            baseProps,
            {
              classes: allClasses,
            },
          ),
        ),
        createComponent(SwitchTrack, {
          get ["class"]() {
            return classes.track;
          },
          ownerState: allProps,
        }),
      ];
    },
  });
});

const TableContext = createContext();

function getTableUtilityClass(slot) {
  return generateUtilityClass("MuiTable", slot);
}
generateUtilityClasses("MuiTable", ["root", "stickyHeader"]);

const $$a = createComponentFactory()({
  name: "MuiTable",
  propDefaults: ({ set }) =>
    set({
      component: defaultComponent$4,
      padding: "normal",
      size: "medium",
      stickyHeader: false,
    }),
  selfPropNames: ["children", "classes", "padding", "size", "stickyHeader"],
  utilityClass: getTableUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", ownerState.stickyHeader && "stickyHeader"],
  }),
});
const TableRoot = styled$1("table", {
  name: "MuiTable",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [styles.root, props.ownerState.stickyHeader && styles.stickyHeader];
  },
})(({ theme, ownerState }) => ({
  display: "table",
  width: "100%",
  borderCollapse: "collapse",
  borderSpacing: 0,
  "& caption": {
    ...theme.typography.body2,
    padding: theme.spacing(2),
    color: theme.palette.text.secondary,
    textAlign: "left",
    captionSide: "bottom",
  },
  ...(ownerState.stickyHeader && {
    borderCollapse: "separate",
  }),
}));
const defaultComponent$4 = "table";

/**
 *
 * Demos:
 *
 * - [Tables](https://mui.com/components/tables/)
 *
 * API:
 *
 * - [Table API](https://mui.com/api/table/)
 */
const Table = $$a.component(function Table({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(TableContext.Provider, {
    value: {
      get padding() {
        return props.padding;
      },
      get size() {
        return props.size;
      },
      get stickyHeader() {
        return props.stickyHeader;
      },
    },
    get children() {
      return createComponent(
        TableRoot,
        mergeProps(
          {
            get role() {
              return allProps.component === defaultComponent$4 ? null : "table";
            },
          },
          otherProps,
          {
            get ["class"]() {
              return clsx(classes.root, otherProps.class);
            },
            ownerState: allProps,
            get children() {
              return props.children;
            },
          },
        ),
      );
    },
  });
});

const Tablelvl2Context = createContext();

function getTableBodyUtilityClass(slot) {
  return generateUtilityClass("MuiTableBody", slot);
}
generateUtilityClasses("MuiTableBody", ["root"]);

const $$9 = createComponentFactory()({
  name: "MuiTableBody",
  propDefaults: ({ set }) =>
    set({
      component: defaultComponent$3,
    }),
  selfPropNames: ["children", "classes"],
  utilityClass: getTableBodyUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const TableBodyRoot = styled$1("tbody", {
  name: "MuiTableBody",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  display: "table-row-group",
});
const defaultComponent$3 = "tbody";

/**
 *
 * Demos:
 *
 * - [Tables](https://mui.com/components/tables/)
 *
 * API:
 *
 * - [TableBody API](https://mui.com/api/table-body/)
 */
const TableBody = $$9.component(function TableBody({
  allProps,
  classes,
  props,
  otherProps,
}) {
  return createComponent(Tablelvl2Context.Provider, {
    value: {
      variant: "body",
    },
    get children() {
      return createComponent(
        TableBodyRoot,
        mergeProps(
          {
            get role() {
              return otherProps.component === defaultComponent$3
                ? null
                : "rowgroup";
            },
          },
          otherProps,
          {
            get ["class"]() {
              return clsx(classes.root, otherProps.class);
            },
            ownerState: allProps,
            get children() {
              return props.children;
            },
          },
        ),
      );
    },
  });
});

function getTableCellUtilityClass(slot) {
  return generateUtilityClass("MuiTableCell", slot);
}
const tableCellClasses = generateUtilityClasses("MuiTableCell", [
  "root",
  "head",
  "body",
  "footer",
  "sizeSmall",
  "sizeMedium",
  "paddingCheckbox",
  "paddingNone",
  "alignLeft",
  "alignCenter",
  "alignRight",
  "alignJustify",
  "stickyHeader",
]);

const $$8 = createComponentFactory()({
  name: "MuiTableCell",
  propDefaults: ({ set }) =>
    set({
      align: "inherit",
    }),
  selfPropNames: [
    "align",
    "children",
    "classes",
    "component",
    "padding",
    "scope",
    "size",
    "sortDirection",
    "variant",
  ],
  autoCallUseClasses: false,
  utilityClass: getTableCellUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.variant,
      ownerState.stickyHeader && "stickyHeader",
      ownerState.align !== "inherit" && `align${capitalize(ownerState.align)}`,
      ownerState.padding !== "normal" &&
        `padding${capitalize(ownerState.padding)}`,
      `size${capitalize(ownerState.size)}`,
    ],
  }),
});
const TableCellRoot = styled$1("td", {
  name: "MuiTableCell",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [
      styles.root,
      styles[props.ownerState.variant],
      styles[`size${capitalize(props.ownerState.size)}`],
      props.ownerState.padding !== "normal" &&
        styles[`padding${capitalize(props.ownerState.padding)}`],
      props.ownerState.align !== "inherit" &&
        styles[`align${capitalize(props.ownerState.align)}`],
      props.ownerState.stickyHeader && styles.stickyHeader,
    ];
  },
})(({ theme, ownerState }) => ({
  ...theme.typography.body2,
  display: "table-cell",
  verticalAlign: "inherit",
  // Workaround for a rendering bug with spanned columns in Chrome 62.0.
  // Removes the alpha (sets it to 1), and lightens or darkens the theme color.
  borderBottom: `1px solid
    ${
      theme.palette.mode === "light"
        ? lighten(alpha(theme.palette.divider, 1), 0.88)
        : darken(alpha(theme.palette.divider, 1), 0.68)
    }`,
  textAlign: "left",
  padding: 16,
  ...(ownerState.variant === "head" && {
    color: theme.palette.text.primary,
    lineHeight: theme.typography.pxToRem(24),
    fontWeight: theme.typography.fontWeightMedium,
  }),
  ...(ownerState.variant === "body" && {
    color: theme.palette.text.primary,
  }),
  ...(ownerState.variant === "footer" && {
    color: theme.palette.text.secondary,
    lineHeight: theme.typography.pxToRem(21),
    fontSize: theme.typography.pxToRem(12),
  }),
  ...(ownerState.size === "small" && {
    padding: "6px 16px",
    [`&.${tableCellClasses.paddingCheckbox}`]: {
      width: 24,
      // prevent the checkbox column from growing
      padding: "0 12px 0 16px",
      "& > *": {
        padding: 0,
      },
    },
  }),
  ...(ownerState.padding === "checkbox" && {
    width: 48,
    // prevent the checkbox column from growing
    padding: "0 0 0 4px",
  }),
  ...(ownerState.padding === "none" && {
    padding: 0,
  }),
  ...(ownerState.align === "left" && {
    textAlign: "left",
  }),
  ...(ownerState.align === "center" && {
    textAlign: "center",
  }),
  ...(ownerState.align === "right" && {
    textAlign: "right",
    flexDirection: "row-reverse",
  }),
  ...(ownerState.align === "justify" && {
    textAlign: "justify",
  }),
  ...(ownerState.stickyHeader && {
    position: "sticky",
    top: 0,
    zIndex: 2,
    backgroundColor: theme.palette.background.default,
  }),
}));

/**
 * The component renders a `<th>` element when the parent context is a header
 * or otherwise a `<td>` element.
 */
const TableCell = $$8.component(function TableCell({
  otherProps,
  allProps,
  props,
}) {
  const table = useContext(TableContext);
  const tablelvl2 = useContext(Tablelvl2Context);
  const isHeadCell = () => tablelvl2 && tablelvl2.variant === "head";
  const component = () => (allProps.component || isHeadCell() ? "th" : "td");
  const scope = () => (!props.scope && isHeadCell() ? "col" : props.scope);
  const variant = () => props.variant || (tablelvl2 && tablelvl2.variant);
  const ariaSort = () =>
    props.sortDirection
      ? props.sortDirection === "asc"
        ? "ascending"
        : "descending"
      : undefined;
  const ownerState = mergeProps(allProps, {
    get component() {
      return component();
    },
    get padding() {
      return (
        props.padding || (table && table.padding ? table.padding : "normal")
      );
    },
    get size() {
      return props.size || (table && table.size ? table.size : "medium");
    },
    get sortDirection() {
      return props.sortDirection;
    },
    get stickyHeader() {
      return variant() === "head" && table && table.stickyHeader;
    },
    get variant() {
      return variant();
    },
  });
  const classes = $$8.useClasses(ownerState);
  const $TableCellRoot = redefine(TableCellRoot, "th");
  return createComponent(
    $TableCellRoot,
    mergeProps(
      {
        get as() {
          return props.component;
        },
        get scope() {
          return scope();
        },
        get ["aria-sort"]() {
          return ariaSort();
        },
      },
      otherProps,
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        ownerState: ownerState,
        get children() {
          return props.children;
        },
      },
    ),
  );
});

function getTableContainerUtilityClass(slot) {
  return generateUtilityClass("MuiTableContainer", slot);
}
generateUtilityClasses("MuiTableContainer", ["root"]);

const $$7 = createComponentFactory()({
  name: "MuiTableContainer",
  propDefaults: ({ set }) =>
    set({
      component: "div",
    }),
  selfPropNames: ["children", "classes"],
  utilityClass: getTableContainerUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const TableContainerRoot = styled$1("div", {
  name: "MuiTableContainer",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  width: "100%",
  overflowX: "auto",
});

/**
 *
 * Demos:
 *
 * - [Tables](https://mui.com/components/tables/)
 *
 * API:
 *
 * - [TableContainer API](https://mui.com/api/table-container/)
 */
const TableContainer = $$7.component(function TableContainer({
  classes,
  allProps,
  props,
  otherProps,
}) {
  return createComponent(
    TableContainerRoot,
    mergeProps(otherProps, {
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: allProps,
      get children() {
        return props.children;
      },
    }),
  );
});

function getTableFooterUtilityClass(slot) {
  return generateUtilityClass("MuiTableFooter", slot);
}
generateUtilityClasses("MuiTableFooter", ["root"]);

const $$6 = createComponentFactory()({
  name: "MuiTableFooter",
  selfPropNames: ["children", "classes"],
  utilityClass: getTableFooterUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const TableFooterRoot = styled$1("tfoot", {
  name: "MuiTableFooter",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  display: "table-footer-group",
});
const tablelvl2$1 = {
  variant: "footer",
};
const defaultComponent$2 = "tfoot";

/**
 *
 * Demos:
 *
 * - [Tables](https://mui.com/components/tables/)
 *
 * API:
 *
 * - [TableFooter API](https://mui.com/api/table-footer/)
 */
const TableFooter = $$6.defineComponent(function TableFooter(inProps) {
  const props = $$6.useThemeProps({
    props: inProps,
  });
  const [, other] = splitProps(props, ["class", "component"]);
  const baseProps = mergeProps(
    {
      component: defaultComponent$2,
    },
    props,
  );
  const ownerState = mergeProps(props, {
    get component() {
      return baseProps.component;
    },
  });
  const classes = $$6.useClasses(ownerState);
  return createComponent(Tablelvl2Context.Provider, {
    value: tablelvl2$1,
    get children() {
      return createComponent(
        TableFooterRoot,
        mergeProps(
          {
            get as() {
              return baseProps.component;
            },
            get ["class"]() {
              return clsx(classes.root, props.class);
            },
            get role() {
              return baseProps.component === defaultComponent$2
                ? null
                : "rowgroup";
            },
            ownerState: ownerState,
          },
          other,
        ),
      );
    },
  });
});

function getTableHeadUtilityClass(slot) {
  return generateUtilityClass("MuiTableHead", slot);
}
generateUtilityClasses("MuiTableHead", ["root"]);

const $$5 = createComponentFactory()({
  name: "MuiTableHead",
  propDefaults: ({ set }) =>
    set({
      component: defaultComponent$1,
    }),
  selfPropNames: ["children", "classes"],
  utilityClass: getTableHeadUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const TableHeadRoot = styled$1("thead", {
  name: "MuiTableHead",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({
  display: "table-header-group",
});
const tablelvl2 = {
  variant: "head",
};
const defaultComponent$1 = "thead";

/**
 *
 * Demos:
 *
 * - [Tables](https://mui.com/components/tables/)
 *
 * API:
 *
 * - [TableHead API](https://mui.com/api/table-head/)
 */
const TableHead = $$5.component(function TableHead({
  classes,
  allProps,
  otherProps,
  props,
}) {
  return createComponent(Tablelvl2Context.Provider, {
    value: tablelvl2,
    get children() {
      return createComponent(
        TableHeadRoot,
        mergeProps(
          {
            get role() {
              return otherProps.component === defaultComponent$1
                ? null
                : "rowgroup";
            },
          },
          otherProps,
          {
            get ["class"]() {
              return clsx(classes.root, otherProps.class);
            },
            ownerState: allProps,
            get children() {
              return props.children;
            },
          },
        ),
      );
    },
  });
});

function getTableRowUtilityClass(slot) {
  return generateUtilityClass("MuiTableRow", slot);
}
const tableRowClasses = generateUtilityClasses("MuiTableRow", [
  "root",
  "selected",
  "hover",
  "head",
  "footer",
]);

const $$4 = createComponentFactory()({
  name: "MuiTableRow",
  propDefaults: ({ set }) =>
    set({
      component: defaultComponent,
      hover: false,
      selected: false,
    }),
  selfPropNames: ["children", "classes", "hover", "selected"],
  utilityClass: getTableRowUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.selected && "selected",
      ownerState.hover && "hover",
      ownerState.head && "head",
      ownerState.footer && "footer",
    ],
  }),
});
const TableRowRoot = styled$1("tr", {
  name: "MuiTableRow",
  slot: "Root",
  overridesResolver: (props, styles) => {
    return [
      styles.root,
      props.ownerState.head && styles.head,
      props.ownerState.footer && styles.footer,
    ];
  },
})(({ theme }) => ({
  color: "inherit",
  display: "table-row",
  verticalAlign: "middle",
  // We disable the focus ring for mouse, touch and keyboard users.
  outline: 0,
  [`&.${tableRowClasses.hover}:hover`]: {
    backgroundColor: theme.palette.action.hover,
  },
  [`&.${tableRowClasses.selected}`]: {
    backgroundColor: alpha(
      theme.palette.primary.main,
      theme.palette.action.selectedOpacity,
    ),
    "&:hover": {
      backgroundColor: alpha(
        theme.palette.primary.main,
        theme.palette.action.selectedOpacity +
          theme.palette.action.hoverOpacity,
      ),
    },
  },
}));
const defaultComponent = "tr";
/**
 * Will automatically set dynamic row height
 * based on the material table element parent (head, body, etc).
 */
/**
 * Will automatically set dynamic row height
 * based on the material table element parent (head, body, etc).
 *
 * Demos:
 *
 * - [Tables](https://mui.com/components/tables/)
 *
 * API:
 *
 * - [TableRow API](https://mui.com/api/table-row/)
 */
const TableRow = $$4.component(function TableRow({
  classes,
  allProps,
  otherProps,
  props,
}) {
  const tablelvl2 = useContext(Tablelvl2Context);
  const ownerState = mergeProps(allProps, {
    get head() {
      return tablelvl2 && tablelvl2.variant === "head";
    },
    get footer() {
      return tablelvl2 && tablelvl2.variant === "footer";
    },
  });
  return createComponent(
    TableRowRoot,
    mergeProps(
      {
        get role() {
          return otherProps.component === defaultComponent ? null : "row";
        },
      },
      otherProps,
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        ownerState: ownerState,
        get children() {
          return props.children;
        },
      },
    ),
  );
});

function getTextFieldUtilityClass(slot) {
  return generateUtilityClass("MuiTextField", slot);
}
generateUtilityClasses("MuiTextField", ["root"]);

const $$3 = createComponentFactory()({
  name: "MuiTextField",
  propDefaults: ({ set }) =>
    set({
      autoFocus: false,
      color: "primary",
      disabled: false,
      error: false,
      fullWidth: false,
      multiline: false,
      required: false,
      select: false,
      variant: "outlined",
    }),
  selfPropNames: [
    "FormHelperTextProps",
    "InputLabelProps",
    "InputProps",
    "autoComplete",
    "autoFocus",
    "children",
    "classes",
    "color",
    "defaultValue",
    "disabled",
    "error",
    "fullWidth",
    "helperText",
    "id",
    "inputProps",
    "inputRef",
    "label",
    "maxRows",
    "minRows",
    "multiline",
    "name",
    "onBlur",
    "onChange",
    "onFocus",
    "placeholder",
    "required",
    "rows",
    "select",
    "size",
    "type",
    "value",
    "variant",
  ],
  utilityClass: getTextFieldUtilityClass,
  slotClasses: () => ({
    root: ["root"],
  }),
});
const variantComponent = {
  standard: Input,
  filled: FilledInput,
  outlined: OutlinedInput,
};
const TextFieldRoot = styled$1(FormControl, {
  name: "MuiTextField",
  slot: "Root",
  overridesResolver: (props, styles) => styles.root,
})({});

/**
 * The `TextField` is a convenience wrapper for the most common cases (80%).
 * It cannot be all things to all people, otherwise the API would grow out of control.
 *
 * ## Advanced Configuration
 *
 * It's important to understand that the text field is a simple abstraction
 * on top of the following components:
 *
 * *   [FormControl](https://mui.com/api/form-control/)
 * *   [InputLabel](https://mui.com/api/input-label/)
 * *   [FilledInput](https://mui.com/api/filled-input/)
 * *   [OutlinedInput](https://mui.com/api/outlined-input/)
 * *   [Input](https://mui.com/api/input/)
 * *   [FormHelperText](https://mui.com/api/form-helper-text/)
 *
 * If you wish to alter the props applied to the `input` element, you can do so as follows:
 *
 * ```jsx
 * const inputProps = {
 *   step: 300,
 * };
 *
 * return <TextField id="time" type="time" inputProps={inputProps} />;
 * ```
 *
 * For advanced cases, please look at the source of TextField by clicking on the
 * "Edit this page" button above. Consider either:
 *
 * *   using the upper case props for passing values directly to the components
 * *   using the underlying components directly as shown in the demos
 *
 * Demos:
 *
 * - [Autocomplete](https://mui.com/components/autocomplete/)
 * - [Pickers](https://mui.com/components/pickers/)
 * - [Text Fields](https://mui.com/components/text-fields/)
 *
 * API:
 *
 * - [TextField API](https://mui.com/api/text-field/)
 * - inherits [FormControl API](https://mui.com/api/form-control/)
 */
const TextField = $$3.component(function TextField({
  allProps,
  classes,
  otherProps,
  props,
}) {
  const id = createUniqueId(() => props.id);
  const helperTextId = () =>
    props.helperText && id() ? `${id()}-helper-text` : undefined;
  const inputLabelId = () =>
    props.label && id() ? `${id()}-label` : undefined;
  const InputComponent = () => variantComponent[props.variant];
  const InputElement = () =>
    createComponent(
      Dynamic,
      mergeProps(
        {
          get $component() {
            return InputComponent();
          },
          get ["aria-describedby"]() {
            return helperTextId();
          },
          get autoComplete() {
            return props.autoComplete;
          },
          get autoFocus() {
            return props.autoFocus;
          },
          get defaultValue() {
            return props.defaultValue;
          },
          get fullWidth() {
            return props.fullWidth;
          },
          get multiline() {
            return props.multiline;
          },
          get name() {
            return props.name;
          },
          get rows() {
            return props.rows;
          },
          get maxRows() {
            return props.maxRows;
          },
          get minRows() {
            return props.minRows;
          },
          get type() {
            return props.type;
          },
          get value() {
            return props.value;
          },
          get id() {
            return id();
          },
          get inputRef() {
            return props.inputRef;
          },
          get onBlur() {
            return props.onBlur;
          },
          get onChange() {
            return props.onChange;
          },
          get onFocus() {
            return props.onFocus;
          },
          get placeholder() {
            return props.placeholder;
          },
          get inputProps() {
            return props.inputProps;
          },
          get size() {
            return props.size;
          },
          get notched() {
            return props.variant === "outlined" &&
              props.InputLabelProps &&
              typeof props.InputLabelProps.shrink !== "undefined"
              ? props.InputLabelProps.shrink
              : undefined;
          },
          get label() {
            return props.variant === "outlined" ? props.label : undefined;
          },
        },
        () => props.InputProps || {},
      ),
    );
  const label = () => {
    const label = props.label;
    return (
      label != null &&
      label !== "" &&
      createComponent(
        InputLabel,
        mergeProps(
          {
            get ["for"]() {
              return id();
            },
            get id() {
              return inputLabelId();
            },
          },
          () => props.InputLabelProps || {},
          {
            children: label,
          },
        ),
      )
    );
  };
  const helperText = () => {
    const helperText = props.helperText;
    if (helperText)
      return createComponent(
        FormHelperText,
        mergeProps(
          {
            get id() {
              return helperTextId();
            },
          },
          () => props.FormHelperTextProps || {},
          {
            children: helperText,
          },
        ),
      );
  };
  return createComponent(
    TextFieldRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(classes.root, otherProps.class);
        },
        get disabled() {
          return props.disabled;
        },
        get error() {
          return props.error;
        },
        get fullWidth() {
          return props.fullWidth;
        },
        get required() {
          return props.required;
        },
        get color() {
          return props.color;
        },
        get variant() {
          return props.variant;
        },
        get size() {
          return props.size;
        },
        ownerState: allProps,
      },
      otherProps,
      {
        get children() {
          return [
            createMemo(() => label()),
            createMemo(() => InputElement()),
            createMemo(() => helperText()),
          ];
        },
      },
    ),
  );
});

const ToggleButtonGroupContext = createContext({});

function isValueSelected(value, candidate) {
  if (candidate === void 0 || value === void 0) {
    return false;
  }
  if (Array.isArray(candidate)) {
    return candidate.indexOf(value) >= 0;
  }
  return value === candidate;
}

function getToggleButtonUtilityClass(slot) {
  return generateUtilityClass("MuiToggleButton", slot);
}
const toggleButtonClasses = generateUtilityClasses("MuiToggleButton", [
  "root",
  "disabled",
  "selected",
  "standard",
  "primary",
  "secondary",
  "sizeSmall",
  "sizeMedium",
  "sizeLarge",
]);

const $$2 = createComponentFactory()({
  name: "MuiToggleButton",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disableFocusRipple",
    "disabled",
    "fullWidth",
    "onChange",
    "onClick",
    "selected",
    "size",
    "value",
  ],
  propDefaults: ({ set }) => {
    const context = useContext(ToggleButtonGroupContext);
    return set({
      get color() {
        return context.color ?? "standard";
      },
      get disabled() {
        return context.disabled ?? false;
      },
      disableFocusRipple: false,
      fullWidth: false,
      get size() {
        return context.size ?? "medium";
      },
    });
  },
  autoCallUseClasses: false,
  utilityClass: getToggleButtonUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      !!ownerState.selected && "selected",
      ownerState.disabled && "disabled",
      ownerState.fullWidth && "fullWidth",
      `size${capitalize(ownerState.size)}`,
      ownerState.color,
    ],
  }),
});
const ToggleButtonRoot = styled$1(ButtonBase, {
  name: "MuiToggleButton",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [styles.root, styles[`size${capitalize(ownerState.size)}`]];
  },
})(({ theme, ownerState }) => {
  const selectedColor =
    ownerState.color === "standard"
      ? theme.palette.text.primary
      : theme.palette[ownerState.color].main;
  return {
    ...theme.typography.button,
    borderRadius: theme.shape.borderRadius,
    padding: 11,
    border: `1px solid ${theme.palette.divider}`,
    color: theme.palette.action.active,
    ...(ownerState.fullWidth && {
      width: "100%",
    }),
    [`&.${toggleButtonClasses.disabled}`]: {
      color: theme.palette.action.disabled,
      border: `1px solid ${theme.palette.action.disabledBackground}`,
    },
    "&:hover": {
      textDecoration: "none",
      // Reset on mouse devices
      backgroundColor: alpha(
        theme.palette.text.primary,
        theme.palette.action.hoverOpacity,
      ),
      "@media (hover: none)": {
        backgroundColor: "transparent",
      },
    },
    [`&.${toggleButtonClasses.selected}`]: {
      color: selectedColor,
      backgroundColor: alpha(
        selectedColor,
        theme.palette.action.selectedOpacity,
      ),
      "&:hover": {
        backgroundColor: alpha(
          selectedColor,
          theme.palette.action.selectedOpacity +
            theme.palette.action.hoverOpacity,
        ),
        // Reset on touch devices, it doesn't add specificity
        "@media (hover: none)": {
          backgroundColor: alpha(
            selectedColor,
            theme.palette.action.selectedOpacity,
          ),
        },
      },
    },
    ...(ownerState.size === "small" && {
      padding: 7,
      fontSize: theme.typography.pxToRem(13),
    }),
    ...(ownerState.size === "large" && {
      padding: 15,
      fontSize: theme.typography.pxToRem(15),
    }),
  };
});

/**
 *
 * Demos:
 *
 * - [Toggle Button](https://mui.com/components/toggle-button/)
 *
 * API:
 *
 * - [ToggleButton API](https://mui.com/api/toggle-button/)
 * - inherits [ButtonBase API](https://mui.com/api/button-base/)
 */

const ToggleButton = $$2.component(function ToggleButton({
  allProps: _allProps,
  otherProps,
  props,
}) {
  const context = useContext(ToggleButtonGroupContext);
  const allProps = mergeProps(_allProps, {
    get selected() {
      return _allProps.selected || isValueSelected(props.value, context.value);
    },
    get fullWidth() {
      return context.fullWidth;
    },
    get onChange() {
      return context.onChange;
    },
  });
  const classes = $$2.useClasses(allProps);
  return createComponent(
    ToggleButtonRoot,
    mergeProps(
      {
        get ["class"]() {
          return clsx(context.class, classes.root, otherProps.class);
        },
        get disabled() {
          return allProps.disabled;
        },
        get focusRipple() {
          return !props.disableFocusRipple;
        },
        onClick: (event) => {
          if (typeof props.onClick === "function") {
            props.onClick(event, props.value);
            if (event.defaultPrevented) {
              return;
            }
          }
          if (typeof allProps.onChange === "function") {
            allProps.onChange(event, props.value);
          }
        },
        get onChange() {
          return allProps.onChange;
        },
        get value() {
          return props.value;
        },
        ownerState: allProps,
        get ["aria-pressed"]() {
          return allProps.selected;
        },
      },
      otherProps,
      {
        get component() {
          return otherProps.component;
        },
        get children() {
          return props.children;
        },
      },
    ),
  );
});

function getToggleButtonGroupUtilityClass(slot) {
  return generateUtilityClass("MuiToggleButtonGroup", slot);
}
const toggleButtonGroupClasses = generateUtilityClasses(
  "MuiToggleButtonGroup",
  [
    "root",
    "selected",
    "vertical",
    "disabled",
    "grouped",
    "groupedHorizontal",
    "groupedVertical",
  ],
);

const $$1 = createComponentFactory()({
  name: "MuiToggleButtonGroup",
  selfPropNames: [
    "children",
    "classes",
    "color",
    "disabled",
    "exclusive",
    "fullWidth",
    "onChange",
    "orientation",
    "size",
    "value",
  ],
  propDefaults: ({ set }) =>
    set({
      color: "standard",
      disabled: false,
      exclusive: false,
      fullWidth: false,
      orientation: "horizontal",
      size: "medium",
    }),
  utilityClass: getToggleButtonGroupUtilityClass,
  slotClasses: (ownerState) => ({
    root: [
      "root",
      ownerState.orientation === "vertical" && "vertical",
      ownerState.fullWidth && "fullWidth",
    ],
    grouped: [
      "grouped",
      `grouped${capitalize(ownerState.orientation)}`,
      ownerState.disabled && "disabled",
    ],
  }),
});
const ToggleButtonGroupRoot = styled$1("div", {
  name: "MuiToggleButtonGroup",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      {
        [`& .${toggleButtonGroupClasses.grouped}`]: styles.grouped,
      },
      {
        [`& .${toggleButtonGroupClasses.grouped}`]:
          styles[`grouped${capitalize(ownerState.orientation)}`],
      },
      styles.root,
      ownerState.orientation === "vertical" && styles.vertical,
      ownerState.fullWidth && styles.fullWidth,
    ];
  },
})(({ ownerState, theme }) => ({
  display: "inline-flex",
  borderRadius: theme.shape.borderRadius,
  ...(ownerState.orientation === "vertical" && {
    flexDirection: "column",
  }),
  ...(ownerState.fullWidth && {
    width: "100%",
  }),
  [`& .${toggleButtonGroupClasses.grouped}`]: {
    ...(ownerState.orientation === "horizontal"
      ? {
          "&:not(:first-of-type)": {
            marginLeft: -1,
            borderLeft: "1px solid transparent",
            borderTopLeftRadius: 0,
            borderBottomLeftRadius: 0,
          },
          "&:not(:last-of-type)": {
            borderTopRightRadius: 0,
            borderBottomRightRadius: 0,
          },
          [`&.${toggleButtonGroupClasses.selected} + .${toggleButtonGroupClasses.grouped}.${toggleButtonGroupClasses.selected}`]:
            {
              borderLeft: 0,
              marginLeft: 0,
            },
        }
      : {
          "&:not(:first-of-type)": {
            marginTop: -1,
            borderTop: "1px solid transparent",
            borderTopLeftRadius: 0,
            borderTopRightRadius: 0,
          },
          "&:not(:last-of-type)": {
            borderBottomLeftRadius: 0,
            borderBottomRightRadius: 0,
          },
          [`&.${toggleButtonGroupClasses.selected} + .${toggleButtonGroupClasses.grouped}.${toggleButtonGroupClasses.selected}`]:
            {
              borderTop: 0,
              marginTop: 0,
            },
        }),
  },
}));

/**
 *
 * Demos:
 *
 * - [Toggle Button](https://mui.com/components/toggle-button/)
 *
 * API:
 *
 * - [ToggleButtonGroup API](https://mui.com/api/toggle-button-group/)
 */
const ToggleButtonGroup = $$1.component(function ToggleButtonGroup({
  allProps,
  classes,
  otherProps,
  props,
}) {
  return createComponent(ToggleButtonGroupContext.Provider, {
    value: {
      get class() {
        return classes.grouped;
      },
      get value() {
        return props.value;
      },
      get size() {
        return props.size;
      },
      get fullWidth() {
        return props.fullWidth;
      },
      get color() {
        return props.color;
      },
      get disabled() {
        return props.disabled;
      },
      onChange: (event, buttonValue) => {
        if (!props.onChange) {
          return;
        } else if (props.exclusive) {
          props.onChange(
            event,
            props.value === buttonValue ? null : buttonValue,
          );
          return;
        }
        const values = props.value;
        const index = values && values.indexOf(buttonValue);
        let newValue;
        if (values && index >= 0) {
          newValue = values.slice();
          newValue.splice(index, 1);
        } else {
          newValue = values ? values.concat(buttonValue) : [buttonValue];
        }
        props.onChange(event, newValue);
      },
    },
    get children() {
      return createComponent(
        ToggleButtonGroupRoot,
        mergeProps(
          {
            role: "group",
            get ["class"]() {
              return clsx(classes.root, otherProps.class);
            },
            ownerState: allProps,
          },
          otherProps,
          {
            get children() {
              return props.children;
            },
          },
        ),
      );
    },
  });
});

function getToolbarUtilityClass(slot) {
  return generateUtilityClass("MuiToolbar", slot);
}
generateUtilityClasses("MuiToolbar", ["root", "gutters", "regular", "dense"]);

const $ = createComponentFactory()({
  name: "MuiToolbar",
  selfPropNames: ["children", "classes", "disableGutters", "variant"],
  propDefaults: ({ set }) =>
    set({
      component: "div",
      disableGutters: false,
      variant: "regular",
    }),
  utilityClass: getToolbarUtilityClass,
  slotClasses: (ownerState) => ({
    root: ["root", !ownerState.disableGutters && "gutters", ownerState.variant],
  }),
});
const ToolbarRoot = styled$1("div", {
  name: "MuiToolbar",
  slot: "Root",
  overridesResolver: (props, styles) => {
    const { ownerState } = props;
    return [
      styles.root,
      !ownerState.disableGutters && styles.gutters,
      styles[ownerState.variant],
    ];
  },
})(
  ({ theme, ownerState }) => ({
    position: "relative",
    display: "flex",
    alignItems: "center",
    ...(!ownerState.disableGutters && {
      paddingLeft: theme.spacing(2),
      paddingRight: theme.spacing(2),
      [theme.breakpoints.up("sm")]: {
        paddingLeft: theme.spacing(3),
        paddingRight: theme.spacing(3),
      },
    }),
    ...(ownerState.variant === "dense" && {
      minHeight: 48,
    }),
  }),
  ({ theme, ownerState }) =>
    ownerState.variant === "regular" && theme.mixins.toolbar,
);

/**
 *
 * Demos:
 *
 * - [App Bar](https://mui.com/components/app-bar/)
 *
 * API:
 *
 * - [Toolbar API](https://mui.com/api/toolbar/)
 */
const Toolbar = $.component(function Toolbar({
  allProps,
  props,
  classes,
  otherProps,
}) {
  return createComponent(
    ToolbarRoot,
    mergeProps(otherProps, {
      get as() {
        return otherProps.component;
      },
      get ["class"]() {
        return clsx(classes.root, otherProps.class);
      },
      ownerState: allProps,
      get children() {
        return props.children;
      },
    }),
  );
});

const mediaPrefix = "@media ";
function useMediaQuery(queryInput) {
  let queryInputString =
    typeof queryInput === "function" ? queryInput(useTheme$1()) : queryInput;
  if (queryInputString.startsWith(mediaPrefix))
    queryInputString = queryInputString.slice(mediaPrefix.length);
  const query = window.matchMedia(queryInputString);
  const [matches, setMatches] = createSignal(query.matches);
  const listener = (e) => setMatches(e.matches);
  if (query.addEventListener) {
    query.addEventListener("change", listener);
  } else {
    query.addListener(listener);
  }
  onCleanup(() => query.removeEventListener("change", listener));
  return matches;
}

// eslint-disable-next-line @typescript-eslint/no-namespace
let PropTypes;
(function (_PropTypes) {})(PropTypes || (PropTypes = {}));

function render$1(code, element, init, options = {}) {
  let disposer;
  createRoot((dispose) => {
    disposer = dispose;
    element === document
      ? code()
      : insert(element, code(), element.firstChild ? null : undefined, init);
  }, options.owner);
  return () => {
    disposer();
    element.textContent = "";
  };
}

// Works
render$1(TextField, document.body);

export {
  Alert,
  AlertTitle,
  AppBar,
  Avatar,
  Backdrop,
  Badge,
  BottomNavigation,
  BottomNavigationAction,
  Box,
  Breadcrumbs,
  Button,
  ButtonBase,
  ButtonGroup,
  Card,
  CardActionArea,
  CardActions,
  CardContent,
  CardHeader,
  CardMedia,
  Checkbox,
  Chip,
  CircularProgress,
  Container,
  CssBaseline,
  Dialog,
  DialogActions,
  DialogContent,
  DialogContentText,
  DialogTitle,
  Divider,
  Drawer,
  Fab,
  Fade,
  FilledInput,
  FormControl,
  FormControlLabel,
  FormGroup,
  FormHelperText,
  FormLabel,
  GlobalStyles,
  Grid,
  Grow,
  Icon,
  IconButton,
  Input,
  InputAdornment,
  InputBase,
  InputLabel,
  LinearProgress,
  Link,
  List,
  ListItem,
  ListItemAvatar,
  ListItemButton,
  ListItemIcon,
  ListItemSecondaryAction,
  ListItemText,
  ListSubheader,
  Menu,
  MenuItem,
  MenuList,
  Modal,
  NativeSelect,
  OutlinedInput,
  Paper,
  Popover,
  Popper,
  PropTypes,
  Radio,
  RadioGroup,
  Select,
  Skeleton,
  Slide,
  Stack,
  StyledEngineProvider,
  SvgIcon,
  Switch,
  SwitchBase,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableFooter,
  TableHead,
  TableRow,
  TextField,
  ThemeProvider,
  ToggleButton,
  ToggleButtonGroup,
  Toolbar,
  Typography,
  alpha,
  createPalette,
  createTheme$1 as createTheme,
  darken,
  decomposeColor,
  emphasize,
  getContrastRatio,
  getLuminance,
  hexToRgb,
  hslToRgb,
  lighten,
  recomposeColor,
  rgbToHex,
  styled$1 as styled,
  useMediaQuery,
  useTheme$1 as useTheme,
  useThemeProps,
};
