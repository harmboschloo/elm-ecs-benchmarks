(function (root, factory) {
  root.BENCHMARKS_HYPERR_ECS = factory();
})(this, function () {
  const api = {};

  // COMPONENTS //

  class ComponentA extends HyperrECS.Component {
    static props = {
      a1: HyperrECS.Types.Number,
    };
  }

  class ComponentB extends HyperrECS.Component {
    static props = {
      b1: HyperrECS.Types.Number,
      b2: HyperrECS.Types.Number,
    };
  }

  class ComponentC extends HyperrECS.Component {
    static props = {
      c1: HyperrECS.Types.Number,
      c2: HyperrECS.Types.Number,
      c3: HyperrECS.Types.Boolean,
    };
  }

  // BENCHMARK Iterate1 //

  class Iterate1System extends HyperrECS.System {
    static queries = {
      a: [ComponentA],
    };

    update() {
      this.queries.a.forEach((entity) => {
        const a = entity.get(ComponentA);
      });
    }
  }

  api.initIterate1 = function (properties) {
    return initWorld3(properties, [Iterate1System]);
  };

  // BENCHMARK Iterate2 //

  class Iterate2System extends HyperrECS.System {
    static queries = {
      ab: [ComponentA, ComponentB],
    };

    update() {
      this.queries.ab.forEach((entity) => {
        const a = entity.get(ComponentA);
        const b = entity.get(ComponentB);
      });
    }
  }

  api.initIterate2 = function (properties) {
    return initWorld3(properties, [Iterate2System]);
  };

  // BENCHMARK Iterate3 //

  class Iterate3System extends HyperrECS.System {
    static queries = {
      abc: [ComponentA, ComponentB, ComponentC],
    };

    update() {
      this.queries.abc.forEach((entity) => {
        const a = entity.get(ComponentA);
        const b = entity.get(ComponentB);
        const c = entity.get(ComponentC);
      });
    }
  }

  api.initIterate3 = function (properties) {
    return initWorld3(properties, [Iterate3System]);
  };

  // BENCHMARK Update1 //

  class Update1System extends HyperrECS.System {
    static queries = {
      a: [ComponentA],
    };

    update() {
      this.queries.a.forEach((entity) => {
        const a = entity.get(ComponentA);

        a.a1 = a.a1 + 1;

        a.modified();
      });
    }
  }

  api.initUpdate1 = function (properties) {
    return initWorld3(properties, [Update1System]);
  };

  // BENCHMARK Update2 //

  class Update2System extends HyperrECS.System {
    static queries = {
      ab: [ComponentA, ComponentB],
    };

    update() {
      this.queries.ab.forEach((entity) => {
        const a = entity.get(ComponentA);
        const b = entity.get(ComponentB);

        a.a1 = a.a1 + 1;
        b.b1 = b.b1 + 1;
        b.b2 = b.b2 - 1;

        a.modified();
        b.modified();
      });
    }
  }

  api.initUpdate2 = function (properties) {
    return initWorld3(properties, [Update2System]);
  };

  // BENCHMARK Update3 //

  class Update3System extends HyperrECS.System {
    static queries = {
      abc: [ComponentA, ComponentB, ComponentC],
    };

    update() {
      this.queries.abc.forEach((entity) => {
        const a = entity.get(ComponentA);
        const b = entity.get(ComponentB);
        const c = entity.get(ComponentC);

        a.a1 = a.a1 + 1;
        b.b1 = b.b1 + 1;
        b.b2 = b.b2 - 1;
        c.c1 = c.c1 + 1;
        c.c2 = c.c2 - 1;
        c.c3 = !c.c3;

        a.modified();
        b.modified();
        c.modified();
      });
    }
  }

  api.initUpdate3 = function (properties) {
    return initWorld3(properties, [Update3System]);
  };

  // WORLD 3 //

  function initWorld3(properties, systems) {
    var world = new HyperrECS.World();
    systems.forEach(function (system) {
      world.systems.register(system);
    });
    world.components.register([ComponentA, ComponentB, ComponentC]);

    for (let i = 0; i < properties.entityCount; i++) {
      world.entities
        .create()
        .add(ComponentA, { a1: 1 })
        .add(ComponentB, { b1: 1, b2: 2 })
        .add(ComponentC, { c1: 1, c2: 2, c3: false })
        .activate();
    }

    return world;
  }

  // UPDATE //

  api.update = function (world) {
    world.update();
  };

  // RETURN //

  return api;
});
