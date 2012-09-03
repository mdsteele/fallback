{- ============================================================================
| Copyright 2011 Matthew D. Steele <mdsteele@alum.mit.edu>                    |
|                                                                             |
| This file is part of Fallback.                                              |
|                                                                             |
| Fallback is free software: you can redistribute it and/or modify it under   |
| the terms of the GNU General Public License as published by the Free        |
| Software Foundation, either version 3 of the License, or (at your option)   |
| any later version.                                                          |
|                                                                             |
| Fallback is distributed in the hope that it will be useful, but WITHOUT     |
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or       |
| FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for   |
| more details.                                                               |
|                                                                             |
| You should have received a copy of the GNU General Public License along     |
| with Fallback.  If not, see <http://www.gnu.org/licenses/>.                 |
============================================================================ -}

module Fallback.Scenario.Triggers.Holmgare
  (compileHolmgare)
where

import Fallback.Scenario.Compile
import Fallback.Scenario.Script
import Fallback.Scenario.Triggers.Globals
import Fallback.Scenario.Triggers.Script
import Fallback.State.Creature (MonsterTownAI(..))
import Fallback.State.Simple (FaceDir(..))
import Fallback.State.Tags
import Fallback.State.Tileset (TileTag(..))

-------------------------------------------------------------------------------

compileHolmgare :: Globals -> CompileScenario ()
compileHolmgare globals = compileArea Holmgare Nothing $ do

  makeExit FrozenPass ["ToFrozenPass1", "ToFrozenPass2"] "FromFrozenPass"
  makeExit SewerCaves ["ToSewerCaves"] "FromSewerCaves"
  makeExit PerilousRoad ["ToPerilousRoad"] "FromPerilousRoad"

  onStartDaily 472927 $ do
    addUnlockedDoors globals
    setAreaCleared Holmgare True

  -- East gate:
  eastGateOpen <- newPersistentVar 198400 False
  trigger 798711 (varTrue eastGateOpen) $ do
    setTerrain AdobeGateOpenTile =<< lookupTerrainMark "EastGate"
  once 448901 (walkIn "NearEastGate") $ conversation $ convNode $ do
    convText "The townspeople have erected a gated wall in the gap in the\
      \ thick trees here.  The construction is rather shoddy--it looks like it\
      \ was put up relatively recently, in a big hurry, and is already\
      \ starting to deteriorate."
    whenP (varFalse eastGateOpen) $ do
      convText "  Still, with the gate closed, it's enough to keep you from\
        \ getting through.  Looks like you'll need to talk to someone in the\
        \ village about having the gate opened before you'll be able to leave\
        \ here."
    whenP (varTrue eastGateOpen) $ do
      convText "\n\n\
        \Sure enough, Mayor Jarmir has already had the gate opened for you. \
        \ Now, you're off to find this lost Sophia girl."

  -- Smithy:
  uniqueDevice 330389 "SmithySign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}GREGOR'S SMITHY{_}"
  simpleTownsperson 217809 TownManApron "Gregor"
                    ImmobileAI $ \_ -> conversation $ do
    let
      aboutSmithy = convNode $ do convText "FIXME"
      whatIsNews = convNode $ do convText "FIXME"
      whatForSale = convNode $ do
        convText "He frowns.  \"Not a lot right now, to be honest,\" he says,\
          \ with apparent regret.  \"I can do repair work, and I can show you\
          \ what little I've got in stock.  But I'm short on raw materials,\
          \ and until I can get more I'm not going to be able to do any\
          \ commission work."
        convChoice canWeHelp "\"You're short on raw materials?  Is that\
          \ something we could help with?\""
        convChoice doShop "\"Well, let's see what you have on hand.\""
      doShop = convNode $ do
        startShopping $ map Right $
          [WeaponItemTag Dagger, WeaponItemTag Shortsword,
           WeaponItemTag Voulge, ArmorItemTag IronMail]
        convText "You conclude your business.  Gregor grunts and turns back to\
          \ his anvil."
      canWeHelp = convNode $ do convText "FIXME"
    convNode $ do
      convText "The blacksmith wipes his brow and sets down his tongs.  \"The\
        \ name's Gregor.  What can I do for you?\""
      convChoice (return ()) "\"I think we're all set.\"  (Leave.)"
      convChoice whatIsNews "\"What's been going on in this village?\""
      convChoice whatForSale "\"What have you got for sale?\""
      convChoice aboutSmithy "\"Tell us about your smithy.\""

  -- Mushroom patch:
  simpleTownsperson 720981 TownWomanRed "Laci"
                    (DrunkAI "LaciZone") $ \_ -> conversation $ do
    let
      whatsWrong = convNode $ convText "Your question just elicits a new round\
        \ of sobbing from the young woman.  She seems to be trying to get\
        \ herself back under control, but isn't ready to answer any questions\
        \ to a stranger just yet."
      waitForHer = convResetNode $ do
        convText "You patiently stand by and give her some quiet company for a\
          \ couple minutes while she gets her tears out.  Finally she quiets\
          \ down a bit, blows her nose on her hankerchief, and starts wiping\
          \ her eyes.  \"I...I'm sorry abo-, about that.  My na-, my name's\
          \ Laci.\"  She blows her nose again, which seems to help her voice. \
          \ \"It's just so awful, what's happened, and now Sophia's gone\
          \ missing.  That poor little girl.  I don't even kn-, I don't even\
          \ know what to wish for--if we don't find her soon, th-, then...but\
          \ if we do, what then?  I can't even imagine what her parents must\
          \ be going through.\"\n\n\
          \Finally mostly recovered, Laci looks around at you and seems to\
          \ become more aware of her surroundings.  \"Why, visitors!  Oh dear,\
          \ I shouldn't be going on like that to you about our village's\
          \ problems.  I...what can I do for you?\""
        convChoice (return ()) "\"We'll be going now.\"  (Leave.)"
        convChoice whereParents "\"Where are Sophia's parents?\""
        convChoice girlMissing "\"A little girl has gone missing?\""
        convChoice whatYouDoing "\"What are you working on here?\""
      whatYouDoing = convNode $ do
        convText "\"Oh!\" she says, looking around at her mushrooms as if she\
          \ had forgotten all about them.  \"I'm just tending the Snow\
          \ Mushrooms here.  They're almost they only thing that will grow\
          \ during the winter here, so we eat a lot of them this time of\
          \ year.\"\n\
          \\n\"They're actually pretty tasty.  I can sell you some of the\
          \ one's I've picked if you'd like to try them.\""
        convChoice buyMushrooms "\"Sure, we'll buy some.\"  (Shop.)"
      buyMushrooms = convNode $ do
        startShopping [Right (PotionItemTag Mushroom)]
        convText "Laci packs the rest of the mushrooms she's collected back\
          \ into her bag, and then wipes a stray tear from her check.  She\
          \ seems to be doing a little better now, at least for the moment."
      girlMissing = convNode $ convText "\"I...\"  Laci looks around; she\
        \ seems suddenly worried.  \"I shouldn't really talk about it, I...\" \
        \ She seems to be trying to make up her mind about something.  \"You\
        \ should just talk to the mayor.  At the town hall.\"  She points to\
        \ the north end of town.  \"Mayor Jarmir will tell you what's\
        \ happened.  Maybe you can help us?\""
      whereParents = convNode $ convText "\"Dorvan and Eithne,\" she answers. \
        \ \"They live in the house in the northwest corner of the village.\" \
        \ She shakes her head.  \"Sophia's their only child.  What must they\
        \ be thinking now?\""
    convNode $ do
      convText "A young woman is walking around the mushroom patches here,\
        \ examining each one, and occasionally picking one that seems to be\
        \ ready and placing it carefully into the small cloth sack she's\
        \ carrying over her shoulder, which is already about half full.  As\
        \ she goes about her work, she is weeping quietly to herself.  As you\
        \ approach, she loses it a bit and has to stop what she is doing in\
        \ order to stand there and sob."
      convChoice (return ()) "\"Er, sorry to interrupt.  We'll be going\" \
        \ (Leave.)"
      convChoice waitForHer "(Wait for her to be ready.)"
      convChoice whatsWrong "\"Why are you crying?\""

  -- Sophia's house:
  simpleTownsperson 711833 TownManRed "Dorvan" ImmobileAI $ \_ge -> do
    narrate "TODO"
  simpleTownsperson 092833 TownWomanBlue "Eithne"
                    ImmobileAI $ \_ -> conversation $ do
    narrate "TODO"

  -- Outdoor guards:
  simpleTownsperson 528013 GuardSmallShield "Kolmancok"
                    (GuardAI 0 "Kolmancok" FaceLeft) $ \_ -> conversation $ do
    narrate "TODO"
  simpleTownsperson 209831 GuardWoman "Reta"
                    (GuardAI 5 "Reta" FaceRight) $ \_ -> conversation $ do
    narrate "TODO"
  simpleTownsperson 502809 GuardSmallShield "Pavel"
                    (PatrolAI "Pavel" "PavelPatrol") $ \_ -> conversation $ do
    narrate "TODO"

  -- Town hall:
  uniqueDevice 490018 "TownHallSign" signRadius $ \_ _ -> do
    narrate "The sign mounted on the wall reads:\n\n\
      \      {b}TOWN HALL{_}"
  simpleTownsperson 309815 GuardLargeShield "Ivan"
                    (GuardAI 5 "Ivan" FaceRight) $ \_ -> conversation $ do
    let
      niceArmor = convNode $ do
        convText "Ivan just grunts again, and says nothing.\n\n\
          \It kind of seemed like an appreciative grunt, though.  He probably\
          \ appreciated your complement, right?  You just hope he'll let you\
          \ side with him in that barfight."
    convNode $ do
      convText "A soldier stands guard by the door here.  You do a slight\
        \ double-take as you walk by him; something seems a little off about\
        \ his proportions.  But quickly you realize that he is simply\
        \ {i}big{_}.  He's about six and a half feet tall, with unbelievibly\
        \ broad shoulders, and biceps about as big around as your thighs.  The\
        \ next time you're choosing sides in a barfight, make sure you're on\
        \ this guy's side.\n\n\
        \He carries a large shield and broadsword, and his armor, though not\
        \ fancy-looking, seems to be of unusually high quality.\n\
        \\n\"Ivan,\" he grunts in response to your greeting.  Even his voice\
        \ reeks of brawn.  \"Talk to th' mayor.\"  He gestures towards the\
        \ seat at the back of the room.  You wait for him to say any more. \
        \ \"Th' mayor,\" he repeats."
      convChoice (return ()) "\"Okay.  We'll do that.\"  (Leave.)"
      convChoice niceArmor "\"That's an impressive-looking suit of armor\
        \ you've got there.\""
  simpleTownsperson 290184 TownManBlue "Jarmir"
                    ImmobileAI $ \_ -> conversation $ do
    narrate "TODO"
    writeVar eastGateOpen True

  -- Tavern:
  simpleTownsperson 470982 TownWomanApron "Marunda"
                    (DrunkAI "MarundaZone") $ \_ -> conversation $ do
    narrate "TODO"
  simpleTownsperson 309810 TownManYellow "Zivon"
                    (GuardAI 5 "Zivon" FaceRight) $ \_ -> conversation $ do
    narrate "TODO"

  -- Tistra's house:
  simpleTownsperson 509834 TownChildBlue "Tistra"
                    (DrunkAI "TistraZone") $ \_ -> conversation $ do
    narrate "TODO"

  -- Graveyard:
  once 558092 (walkIn "Graveyard") $ do
    narrate "You find the village graveyard tucked away behind the trees\
      \ here.  The gravestones, and the little cobblestone pavement at the\
      \ center, are actually quite lovely.  Despite the troubles that this\
      \ village must be facing, it seems that they really put effort into\
      \ honoring their dead, and each other."
  uniqueDevice 779109 "Grave1" signRadius $ \_ _ -> do
    narrate "The carving on the gravestone reads:\n\n\
      \          {b}SVELA BARTHOLD{_}\n\
      \                   1219-1271\n\n\
      \      {i}The people of Holmgare{_}\n\
      \        {i}will always remember{_}\n\
      \               {i}your kindness.{_}\n\n\
      \It would seem, from the dates, that this particular grave was put here\
      \ only a few years ago."
  uniqueDevice 806981 "Grave2" signRadius $ \_ _ -> do
    narrate "The carving on the gravestone reads:\n\n\
      \             {b}ALBION DORVA{_}\n\
      \                   1086-1160\n\n\
      \      {i}Tenth mayor of Holmgare{_}\n\
      \       {i}\"The one who endures to{_}\n\
      \         {i}the end will be saved.\"{_}"

-------------------------------------------------------------------------------
