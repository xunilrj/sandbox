# Unreal 4 and Globalization (Localization)

This a small tutorial that touches some interesting aspects of the Unreal Engine.

First we will use the out-of-the-box Third person game template;  
Second we will use triggers; they allow you to do something when an actor enter/exit its volume;  
Thirs we will create a very simple UI.
Forth we will globalize/localize the "game" to English and Portuguese.

# Template

![alt text](./001.gif "")
![alt text](./002.gif "")
![alt text](./003.gif "")

# Trigger

In the "Models" tab, go to "Basic" and choose "Sphere Trigger". I chose to put this trigger around the "spinning coin", so when the player reaches closes enough (1.8 meters) we will show some information about that area.

![alt text](./004.gif "")

The importante part here is the "Sphere Radius". I chose 180.

![alt text](./005.gif "")

When you created the trigger, Unreal chose the default name "TriggerSphere", find it in the "World Outliner", where all objects are listed, right-click the trigger and choose "Add Event -> OnActorBeginOverlap".

This will allow us to do something when an actor, in our case our player enters this volume. Ideally we have to filter the action based on who is entering the volume. We will deal with this when we move this "code" to c++.

![alt text](./006.gif "")

Unreal opened the "Level Blueprint" for us, if we drag the "white pin" of "OnActorBeginOverlap" and choose "Print Text", we can prinmt a simple text when the actor enters the area.

Hit play now and see it in action.

There is a important concept here. Unreal has two similar but very different classes for text. FText and FString. That is why we chose the "Print Text" and not "Print String", which also exist.

The bottom line is "Texts are for globalized text". We can translate them.

![alt text](./007.gif "")

But we can do better. Now we will, instaead of just printing a string, we will show an widget, an more sophisticated UI and remove it when the player leaves the area.

Later we can improve this UI with a better aesthetic and some transitions.

First we need to create the widget: "Add New -> User Interface -> Widget Blueprint".

![alt text](./008.gif "")

Here I am naming this UI as "ShowInfoUI"

![alt text](./009.gif "")

Then drag a Text to anywhere inside the canvas. Type the text you want to show to the player.

![alt text](./010.gif "")

I decided to anchor the text in the "top border".

![alt text](./011.gif "")

![alt text](./012.gif "")

With the ancor controlling its measure I zeroed all offsets and sizes...

![alt text](./013.gif "")

and chose the text to be centered (Justification).

![alt text](./014.gif "")

If you click in the little arrow beside the "Text" property, you will see the globalization/localization details. Check that is "Localizable". I also chose a namespace and a easier key. They will be useful when being used from C++ classes.

![alt text](./015.gif "")

After this let us return to the "Level Blueprint" and change our reaction to the "TriggerSphere.OnActorBeginOverlap", the event that is triggered when we enter the volume, to create the widget.

We use the "Create Widget" node. Choose your widge in the property "Class".

![alt text](./016.gif "")

This node does not show the widget, it justs creates it. To show it we need first to assign the return value from "Create" to a variable. And then show this variable.

We can chain the creation and display together, but we will need the variable to close the widget.

![alt text](./017.gif "")

With the variable created, we see a "Set Node", that sets the value of a "Blueprint Variable". After this we can chain this variable to the "Add to Viewport" node. This node is the one who "shows" our UI.

![alt text](./018a.gif "")

We can se our "Text Box" anchored at th top of the screen. Humble, but it works.
Now, would be very great if we could remove this as soon as the player leaves the SphereTrigger volume.

![alt text](./020.gif "")

To do this, return to the TriggerSphere menu on the "World Outlier" and choose "Add Event -> OnActorEndOverlap".

![alt text](./021.gif "")

The difference now is that we need to drag the "ui" variable from the "Variable" section to the "Blueprint" and choose "Get ui".

![alt text](./022.gif "")

After this we can call the "Remove from Parent" node.

![alt text](./023.gif "")

and set the "Target" property as the "ui" variable.

![alt text](./024.gif "")

Now we are ready to start translating our strings.

# Translation

Now go to "Window -> Localization Dashboard".

![alt text](./026.gif "")

Inside the "Localization Dashboard", click in the almost invisible "Radio Button" on the column "Native" besides thre "English" culture on the bottom of the screen.

This will setup the "English" language as the default. This will tell "Unreal" to gather all of the text of the solution and assume it is already in English.

![alt text](./028.gif "")

Then find the "Gather Text" section on the same window. Here you will define where the Unreal will search for "Text".

We will search "From Packages" in the "Content" folder. You can choose whatever makes sense in your case here, off course.

![alt text](./036.gif "")

We can click in the "Gather Text" button now.

![alt text](./029.gif "")

It will open a window that takes some time to find all texts.

![alt text](./030.gif "")

After this we can create another "Culture" clicking in the "Add New Culture". In my case here, "Portuguese".

![alt text](./032.gif "")

With the new culture created, we can start to translate the texts. The first step is to click in the first icon, in the "Actions" column.

![alt text](./034.gif "")

If we switch to the "Untranslated" tab. We will find our strings. In my case just two.

![alt text](./037.gif "")



![alt text](./038.gif "")

You can easily translate them here. Remember to "Save".

![alt text](./039.gif "")

After this you need to "Compile Text". Very important and very easy to forget. Otherwise it will not work.

![alt text](./040.gif "")

# Language Switch

We will implement a more realistic "Language" switch later.

For now we just need to change the language to see if everything is working. So we will just implement some reactions to the keyboard.

In the "Level Blueprint" search for "Keyboard Events -> 1".

![alt text](./041.gif "")

After this chain this node to "Set Current Language". In the "Culture" property type the two letter code for your language. For "Portuguese" it is "pt". For "English" it is "en".

I have also chained the execution to a "Print String" to show the current language. Just to certify that everything worked.

![alt text](./042.gif "")

Now this is the result.

Warning: Sometimes "playing inside" the Unreal does not show the translation. That is why I testes with a detached Window.

![alt text](./043.opt.gif "")

