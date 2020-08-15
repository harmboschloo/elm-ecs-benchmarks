(function (root, factory) {
  root.ECSY_BENCHMARKS = factory();
})(this, function () {
  const api = {};

  // COMPONENTS //

  class ComponentA extends ECSY.Component {}

  ComponentA.schema = {
    a1: { type: ECSY.Types.Number },
  };

  class ComponentB extends ECSY.Component {}

  ComponentB.schema = {
    b1: { type: ECSY.Types.Number },
    b2: { type: ECSY.Types.Number },
  };

  class ComponentC extends ECSY.Component {}

  ComponentC.schema = {
    c1: { type: ECSY.Types.Number },
    c2: { type: ECSY.Types.Number },
    c3: { type: ECSY.Types.Boolean },
  };

  // BENCHMARK Iterate1 //

  class Iterate1System extends ECSY.System {
    execute(delta, time) {
      this.queries.a.results.forEach((entity) => {
        const a = entity.getComponent(ComponentA);
      });
    }
  }

  Iterate1System.queries = {
    a: {
      components: [ComponentA],
    },
  };

  api.initIterate1 = function (properties) {
    return initWorld3(properties, [Iterate1System]);
  };

  // BENCHMARK Iterate2 //

  class Iterate2System extends ECSY.System {
    execute(delta, time) {
      this.queries.ab.results.forEach((entity) => {
        const a = entity.getComponent(ComponentA);
        const b = entity.getComponent(ComponentB);
      });
    }
  }

  Iterate2System.queries = {
    ab: {
      components: [ComponentA, ComponentB],
    },
  };

  api.initIterate2 = function (properties) {
    return initWorld3(properties, [Iterate2System]);
  };

  // BENCHMARK Iterate3 //

  class Iterate3System extends ECSY.System {
    execute(delta, time) {
      this.queries.abc.results.forEach((entity) => {
        const a = entity.getComponent(ComponentA);
        const b = entity.getComponent(ComponentB);
        const c = entity.getComponent(ComponentC);
      });
    }
  }

  Iterate3System.queries = {
    abc: {
      components: [ComponentA, ComponentB, ComponentC],
    },
  };

  api.initIterate3 = function (properties) {
    return initWorld3(properties, [Iterate3System]);
  };

  // BENCHMARK Update1 //

  class Update1System extends ECSY.System {
    execute(delta, time) {
      this.queries.a.results.forEach((entity) => {
        const a = entity.getMutableComponent(ComponentA);

        a.a1 = a.a1 + 1;
      });
    }
  }

  Update1System.queries = {
    a: {
      components: [ComponentA],
    },
  };

  api.initUpdate1 = function (properties) {
    return initWorld3(properties, [Update1System]);
  };

  // BENCHMARK Update2 //

  class Update2System extends ECSY.System {
    execute(delta, time) {
      this.queries.ab.results.forEach((entity) => {
        const a = entity.getMutableComponent(ComponentA);
        const b = entity.getMutableComponent(ComponentB);

        a.a1 = a.a1 + 1;
        b.b1 = b.b1 + 1;
        b.b2 = b.b2 - 1;
      });
    }
  }

  Update2System.queries = {
    ab: {
      components: [ComponentA, ComponentB],
    },
  };

  api.initUpdate2 = function (properties) {
    return initWorld3(properties, [Update2System]);
  };

  // BENCHMARK Update3 //

  class Update3System extends ECSY.System {
    execute(delta, time) {
      this.queries.abc.results.forEach((entity) => {
        const a = entity.getMutableComponent(ComponentA);
        const b = entity.getMutableComponent(ComponentB);
        const c = entity.getMutableComponent(ComponentC);

        a.a1 = a.a1 + 1;
        b.b1 = b.b1 + 1;
        b.b2 = b.b2 - 1;
        c.c1 = c.c1 + 1;
        c.c2 = c.c2 - 1;
        c.c3 = !c.c3;
      });
    }
  }

  Update3System.queries = {
    abc: {
      components: [ComponentA, ComponentB, ComponentC],
    },
  };

  api.initUpdate3 = function (properties) {
    return initWorld3(properties, [Update3System]);
  };

  // WORLD 3 //

  function initWorld3(properties, systems) {
    var world = new ECSY.World();
    world
      .registerComponent(ComponentA)
      .registerComponent(ComponentB)
      .registerComponent(ComponentC);

    systems.forEach(function (system) {
      world.registerSystem(system);
    });

    for (let i = 0; i < properties.entityCount; i++) {
      world
        .createEntity()
        .addComponent(ComponentA, { a1: 1 })
        .addComponent(ComponentB, { b1: 1, b2: 2 })
        .addComponent(ComponentC, { c1: 1, c2: 2, c3: false });
    }

    return world;
  }

  // UPDATE //

  api.update = function (world) {
    world.execute(0, 0);
  };

  // RETURN //

  return api;
});
