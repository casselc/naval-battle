let's frame this as overhead isometric view

We'll be making a naval combat game, where the ocean is a particle simulation, and the ships are made of voxels. The first prototype will be of just the player
against another ship shooting at one another until one sinks

Think about a massive physics based naval combat game where you build giant destructible voxel dreadnoughts and fight in a fully simulated turbulent ocean. Instead of calculating gravity in a space vacuum you use those same two algorithms to perfectly simulate buoyancy and water pressure. When a torpedo blows a massive hole in the side of your ship the physical geometry of your hull is permanently altered. The engine uses the divergence theorem on your newly broken surface mesh to instantly recalculate your exact displaced volume and your shifting center of buoyancy. This means your ship lists and sinks with absolute physical accuracy based strictly on the jagged new shape of the breached hull without the engine needing to count the thousands of flooded interior blocks.

While that surface volume trick handles the structural buoyancy the Fast Multipole Method would be put to work simulating the actual water. FMM is incredibly powerful for calculating incompressible fluid dynamics using vortex particle methods. Instead of a flat polygon plane of water with a scrolling texture you could have an ocean made of millions of individually interacting fluid particles. Because FMM groups distant fluid particles together to approximate their flow and pressure you can simulate massive rogue waves and chaotic whirlpools crashing against your fleet without melting your processor.

## future ideas

The interaction between those two systems would create a ridiculously fun sandbox. You could ram a reinforced submarine through a submerged voxel iceberg and watch the ice dynamically shatter into thousands of pieces. The divergence algorithm immediately updates the mass and buoyancy of every newly broken ice chunk so they bob and spin to the surface realistically. At the same time the FMM driven water simulation violently rushes into the empty space left behind by the shattered ice creating a localized undertow that drags smaller ship debris around. You get a completely dynamic fluid and structural physics engine capable of handling massive fleet battles and catastrophic sinking events in real time.

use wave function collapse to generate the map

## resources

use the following resources for reference 

/Users/yogthos/src/jolt-lang/voxel-siege
fast-volume.html
~/src/fast-multipole-method

/Users/yogthos/src/wave-function-collapse
