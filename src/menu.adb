-- 1. Choisir un arbre
-- 2. Créer un arbre
-- 3. Supprimer un arbre
-- 4. Changer la verbosité (4 actuellement)
-- 5. Quitter

-- 1. Ajouter un ancêtre
-- 2. Modifier les données d'un ancêtre
-- 3. Supprimer un ancêtre
-- 4. Obtenir le nombre d'ancêtres connus d'un individu
-- 5. Obtenir l'ensemble d'ancêtres de génération N d'un individu
-- 6. Afficher l'arbre à partir d'un noeud donné
-- 7. Obtenir les individus sans parents connus
-- 8. Obtenir les individus avec un seul parent connu
-- 9. Obtenir les individus avec deux parents
-- 10. Supprimer l'arbre
-- 11. Quitter

-- BIG TODO : handle case when user puts "1 2" when asked for prompt

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with FamilyTree;            use FamilyTree;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with utils; use utils;
with Person; use Person;

procedure Menu is

    type TreeTuple is record
        Name: Unbounded_String;
        Tree: T_FamilyTree;
    end record;

    package TreeVector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => TreeTuple);
    use TreeVector;
    ExistingTrees  : TreeVector.Vector;
    CurrentTree    : TreeTuple;
    Verbosity      : Integer := 1;
    ExitMainMenu   : Boolean := False;
    MainMenuChoice : Integer := 0;
    ShowMainMenu   : Boolean := True;
    QuitCharacter  : Unbounded_String := To_Unbounded_String ("q");

    type String_Access is Access all Unbounded_String;
    type Boolean_Access is Access all Boolean;
    type Person_Access is Access all T_Person;

    ExitCreateNewPersonMenu : aliased Boolean := False;
    ExitCreateNewTreeMenu : aliased Boolean := False;
    
    FirstName : aliased Unbounded_String;
    LastName : aliased Unbounded_String;
    Gender : aliased Unbounded_String;
    Birthdate : aliased Unbounded_String;

    NewPerson : aliased T_Person;
    NewPersonKey : aliased Integer;
    NewTreeName : aliased Unbounded_String;

    QUIT_FLAG_CREATE_PERSON :  Boolean := False;

    procedure TreeMenu is
        procedure HandleAddAncestor is
        begin
            null;
        end HandleAddAncestor;

        procedure HandleModifyAncestor is
        begin
            null;
        end HandleModifyAncestor;

        procedure HandleDeleteAncestor is
        begin
            null;
        end HandleDeleteAncestor;

        procedure HandleGetAncestorsCount is
        begin
            null;
        end HandleGetAncestorsCount;

        procedure HandleGetAncestorsByGeneration is
        begin
            null;
        end HandleGetAncestorsByGeneration;

        procedure HandleShowTree is
        begin
            null;
        end HandleShowTree;

        procedure HandleGetOrphanIndividuals is
        begin
            null;
        end HandleGetOrphanIndividuals;

        procedure HandleGetSingleParentIndividuals is
        begin
            null;
        end HandleGetSingleParentIndividuals;

        procedure HandleGetDualParentIndividuals is
        begin
            null;
        end HandleGetDualParentIndividuals;

        procedure HandleCleanTree is
        begin
            null;
        end HandleCleanTree;

        Choice : Integer;
        ExitTreeMenu : Boolean := False;
        ChosenTree : T_FamilyTree;

    begin
        Put_Line("check");
        ChosenTree := CurrentTree.Tree;
        showFamilyTree (ChosenTree, 4);
        New_Line;
        Put_Line("vector length: " & Integer(ExistingTrees.Length)'Image);
        Put_Line("check");
        while not ExitTreeMenu loop
            New_Line;
            Put_Line ("--- Gestion de l'arbre " & To_String(CurrentTree.Name) & " ---");
            New_Line;
            showFamilyTree (CurrentTree.Tree, Verbosity => 2);
            New_Line;
            Put_Line ("1. Afficher l'arbre avec la verbosité " & getTrimmedInt(Verbosity));
            Put_Line ("2. Afficher l'arbre à partir d'un noeud donne");
            Put_Line ("3. Ajouter un ancêtre");
            Put_Line ("5. Supprimer un ancêtre");
            Put_Line ("6. Obtenir le nombre d'ancêtres connus d'un individu");
            Put_Line ("7. Obtenir l'ensemble des ancêtres de generation N d'un individu");
            Put_Line ("8. Obtenir les individus sans parents connus");
            Put_Line ("9. Obtenir les individus avec un seul parent connu");
            Put_Line ("10. Obtenir les individus avec deux parents");
            Put_Line ("11. Retourner au menu principal");

            New_Line;
            Put ("Entrez votre choix (1-11) : ");
            Get (Item => Choice);

            case Choice is
                when 1 =>
                    showFamilyTree (CurrentTree.Tree, Verbosity);
                when 2 =>
                    HandleModifyAncestor;
                when 3 =>
                    HandleDeleteAncestor;
                when 4 =>
                    HandleGetAncestorsCount;
                when 5 =>
                    HandleGetAncestorsByGeneration;
                when 6 =>
                    HandleShowTree;
                when 7 =>
                    HandleGetOrphanIndividuals;
                when 8 =>
                    HandleGetSingleParentIndividuals;
                when 9 =>
                    HandleGetDualParentIndividuals;
                when 10 =>
                    HandleCleanTree;
                when 11 =>
                    ExitTreeMenu := True;
                when others =>
                    Put_Line ("Choix invalide, veuillez réessayer (1-11).");
            end case;
        end loop;

    end TreeMenu;

    procedure HandleInput(Pointer : in String_Access; Stop : in Boolean_Access; TextString : in String) is
        ExitInput : Boolean := False;
        Input : Unbounded_String;
    begin
        while not ExitInput loop
            begin
                Put (TextString);
                Input := To_Unbounded_String(Get_Line);

                if Input = QuitCharacter then
                    ExitInput := True;
                    Stop.all := True;
                else
                    Pointer.all := Input;
                    ExitInput := True;
                end if;
            exception
                when Data_Error =>
                    Skip_Line;
                    New_Line;
                    Put(getColoredString("Saisie invalide, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ", ERROR));
            end;
        end loop;
    end HandleInput;

    function getKeyByInput(mustBePresent : in Boolean; checkPresence : in Boolean := True) return Integer is
        -- isPresent if the key is supposed to be present
        -- not isPresent if the key is supposed to be absent
        ExitGetKey : Boolean := False;
        ShowGetKeyMenu : Boolean := True;
        IntegerKey : Integer;
        Key : Unbounded_String;

        procedure Handle_Exception(Message : in String) is
        begin
            Skip_Line;
            New_Line;
            Put(getColoredString(Message, ERROR));
            ShowGetKeyMenu := False;
        end Handle_Exception;
    begin
        while not ExitGetKey loop
            begin
                if ShowGetKeyMenu then
                    Put ("Entrez la clé: ");
                end if;
                Key := To_Unbounded_String(Get_Line);

                if not (Key = QuitCharacter) then
                    IntegerKey := Integer'Value(To_String(Key));

                    if checkPresence then
                        if mustBePresent then
                            if not isPresent(CurrentTree.Tree, IntegerKey) then
                                raise Absent_Key_Exception;
                            end if;
                        else
                            if isPresent(CurrentTree.Tree, IntegerKey) then
                                raise Present_Key_Exception;
                            end if;
                        end if;
                    end if;

                    return IntegerKey;
                else
                    return -1;
                end if;
            exception
                when Data_Error | Constraint_Error =>
                    Handle_Exception("Saisie invalide, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ");
                when Present_Key_Exception =>
                    Handle_Exception("La clé " & To_String(Key) & " est déjà présente dans l'arbre, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ");
                when Absent_Key_Exception =>
                    Handle_Exception("La clé " & To_String(Key) & " n'est pas présente dans l'arbre, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ");
            end;
        end loop;
        return -1;
    end getKeyByInput;

    function getExistingTreesLength return Integer is
    begin
        return Integer(ExistingTrees.Length) + 1;
    end getExistingTreesLength;

    procedure AddDefaultTree is
        Tuple : TreeTuple;

        function getExampleTree return T_FamilyTree is
            FamilyTree : T_FamilyTree;
        begin
            initChild(FamilyTree, 1, Person.initPersonObj(
                FirstName => To_Unbounded_String("Victor"),
                LastName => To_Unbounded_String("Wembanyama"),
                Gender => To_Unbounded_String("Male"),
                Birthdate => To_Unbounded_String("04-01-2004")
            ));
            addAncestor(FamilyTree, 1, LEFT, 2, Person.initPersonObj(
                FirstName => To_Unbounded_String("LeBron"),
                LastName => To_Unbounded_String("James"),
                Gender => To_Unbounded_String("Male"),
                Birthdate => To_Unbounded_String("30-12-1984")
            ));
            addAncestor(FamilyTree, 1, RIGHT, 3, Person.initPersonObj(
                FirstName => To_Unbounded_String("Lisa"),
                LastName => To_Unbounded_String("Leslie"),
                Gender => To_Unbounded_String("Female"),
                Birthdate => To_Unbounded_String("07-07-1972")
            ));
            addAncestor(FamilyTree, 3, LEFT, 4, Person.initPersonObj(
                FirstName => To_Unbounded_String("Kobe"),
                LastName => To_Unbounded_String("Bryant"),
                Gender => To_Unbounded_String("Male"),
                Birthdate => To_Unbounded_String("23-08-1978")
            ));
            addAncestor(FamilyTree, 4, RIGHT, 5, Person.initPersonObj(
                FirstName => To_Unbounded_String("Michael"),
                LastName => To_Unbounded_String("Jordan"),
                Gender => To_Unbounded_String("Male"),
                Birthdate => To_Unbounded_String("17-02-1963")
            ));

            return FamilyTree;
        end getExampleTree;
    begin
        Tuple.Name := To_Unbounded_String("Arbre exemple");
        Tuple.Tree := getExampleTree;
        ExistingTrees.Append (Tuple);
    end AddDefaultTree;
    
    procedure AddNewTree(Name : in String) is
        Tuple : TreeTuple;
    begin
        Tuple.Name := To_Unbounded_String(Name);
        Tuple.Tree := getEmptyFamilyTree;
        ExistingTrees.Append (Tuple);
    end AddNewTree;

    procedure ShowExistingTrees is
        Index : Integer := 1;
    begin
        for el of ExistingTrees loop
            Put_Line (Index'Img & ". " & To_String(el.Name));
            Index := Index + 1;
        end loop;
    end ShowExistingTrees;

    procedure HandleCreateNewPerson(TargetKey : in Integer := 0; Position : in T_Position := ROOT) is
        ShowCreateNewPersonMenu : Boolean := True;
        ShowKeyText : Boolean := True;
    begin
        ExitCreateNewPersonMenu := False;
        FirstName := To_Unbounded_String("");
        LastName := To_Unbounded_String("");
        Gender := To_Unbounded_String("");
        Birthdate := To_Unbounded_String("");

        while not ExitCreateNewPersonMenu loop
            if ShowCreateNewPersonMenu then
                New_Line;
                if Position = ROOT then
                    Put_Line ("--- Ajoute de l'enfant ---");
                else
                    Put_Line ("--- Ajout d'un nouvel ancêtre ---");
                end if;
                New_Line;
            end if;
            begin
                NewPersonKey := getKeyByInput(False, False);
                
                if NewPersonKey = -1 then
                    ExitCreateNewPersonMenu := True;
                end if;

                if not ExitCreateNewPersonMenu then
                    if not ExitCreateNewPersonMenu then HandleInput (Pointer => FirstName'Access, Stop => ExitCreateNewPersonMenu'Access, TextString => "Entrez le prénom de la personne: "); end if;
                    if not ExitCreateNewPersonMenu then HandleInput (Pointer => LastName'Access, Stop => ExitCreateNewPersonMenu'Access, TextString => "Entrez le nom de la personne: "); end if;
                    if not ExitCreateNewPersonMenu then HandleInput (Pointer => Gender'Access, Stop => ExitCreateNewPersonMenu'Access, TextString => "Entrez le sexe de la personne: "); end if;
                    if not ExitCreateNewPersonMenu then HandleInput (Pointer => Birthdate'Access, Stop => ExitCreateNewPersonMenu'Access, TextString => "Entrez la date d'anniversaire de la personne: "); end if;
                    
                    if not ExitCreateNewPersonMenu then
                        NewPerson := initPersonObj(
                            FirstName => FirstName, 
                            LastName => LastName, 
                            Gender => Gender, 
                            BirthDate => Birthdate
                        );
                    end if;
                end if;

                if ExitCreateNewPersonMenu then
                    QUIT_FLAG_CREATE_PERSON := True;
                    New_Line;
                    if Position /= ROOT then
                        Put_Line(getColoredString("Abandon de l'operation. L'ancêtre n'a pas été ajouté.", WARNING));
                    else
                        Put_Line(getColoredString("Abandon de l'operation. L'arbre n'a pas été crée.", WARNING));
                    end if;
                else 
                    if Position /= ROOT then
                        New_Line;
                        Put_Line(getColoredString("Une nouvelle personne a été ajoutée", SUCCESS));
                    end if;
                end if;
                ExitCreateNewPersonMenu := True;
            exception
                when Data_Error | Constraint_Error =>
                    Skip_Line;
                    New_Line;
                    Put(getColoredString("Saisie invalide, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ", ERROR));
                    ShowCreateNewPersonMenu := False;
                    ShowKeyText := False;
            end;
        end loop;
        ExitCreateNewPersonMenu := False;
    end HandleCreateNewPerson;

    procedure HandleChooseTree(CurrentTree : in out TreeTuple) is
        ExitChooseTreeMenu : Boolean := False;
        ShowChooseTreeMenu : Boolean := True;
        ChoicesIndex : String := "(1-" & Trim(getExistingTreesLength'Image, Ada.Strings.Left) & ")";
    begin
        if getExistingTreesLength - 1 = 0 then
            New_Line;
            Put_Line(getColoredString("Aucun arbre à choisir. Veuillez d'abord créer un arbre.", WARNING));
            ExitChooseTreeMenu := True;
        end if;

        while not ExitChooseTreeMenu loop
            if ShowChooseTreeMenu then
                New_Line;
                Put_Line ("--- Choisir un arbre ---");
                New_Line;
                ShowExistingTrees;
                Put_Line (getExistingTreesLength'Image & ". Retourner au menu principal");
                New_Line;
                Put ("Entrez votre choix " & ChoicesIndex & ": ");
            end if;
            declare
                TreeToChooseIndex : Integer;
            begin
                Get (Item => TreeToChooseIndex);
                if TreeToChooseIndex < 0 or TreeToChooseIndex > getExistingTreesLength then
                    raise Data_Error;
                else
                    if not (TreeToChooseIndex = getExistingTreesLength) then
                        CurrentTree := ExistingTrees.Element (TreeToChooseIndex - 1);
                        New_Line;
                        Put_Line(getColoredString("L'arbre '" & To_String(CurrentTree.Name) & "' a été choisi.", SUCCESS)); 
                        TreeMenu;
                        --  addAncestor (ABR => CurrentTree.Tree, TargetKey => 1, Position => LEFT, NewKey => 2, NewPerson => initPersonObj);
                    end if;
                end if;
                ExitChooseTreeMenu := True;
                ShowMainMenu := True;
            exception
                when Data_Error =>
                    Skip_Line;
                    New_Line;
                    Put(getColoredString("Choix invalide, veuillez réessayer " & ChoicesIndex & ": ", ERROR));
                    ShowChooseTreeMenu := False;
            end;
        end loop;
    end HandleChooseTree;

    procedure HandleCreateTree(CurrentTree : in out TreeTuple) is
        Tree : T_FamilyTree;
    begin
        ExitCreateNewTreeMenu := False;
        while not ExitCreateNewTreeMenu loop
            Skip_Line;
            New_Line;
            Put_Line ("--- Créer un arbre ---");
            New_Line;
            HandleInput (Pointer => NewTreeName'Access, Stop => ExitCreateNewTreeMenu'Access, TextString => "Entrez le nom de l'arbre à créer ('q' pour quitter): ");

            if not ExitCreateNewTreeMenu then
                AddNewTree (To_String(NewTreeName));
                HandleCreateNewPerson;

                if not QUIT_FLAG_CREATE_PERSON then
                    CurrentTree := ExistingTrees.Element(Index => Integer(ExistingTrees.Length) - 1);
                    Tree := CurrentTree.Tree;
                    initChild (Tree, NewPersonKey, NewPerson);
                    showFamilyTree (Tree);
                    New_Line;
                    Put_Line(getColoredString("L'arbre " & To_String(CurrentTree.Name) & " a été crée.", SUCCESS));
                end if;
            else
                Put_Line(getColoredString("Abandon de l'operation. L'arbre n'a pas été crée'.", WARNING));
            end if;

            ExitCreateNewTreeMenu := True;
        end loop;
    end HandleCreateTree;

    procedure HandleDeleteTree is
        ExitDeleteTreeMenu : Boolean := False;
        ShowDeleteTreeMenu : Boolean := True;
        getExistingTreesLength : Integer := Integer(ExistingTrees.Length) + 1;
        ChoicesIndex : String := "(1-" & Trim(getExistingTreesLength'Image, Ada.Strings.Left) & ")";
    begin
        if getExistingTreesLength - 1 = 0 then
            New_Line;
            Put_Line(getColoredString("Aucun arbre à supprimer. Veuillez d'abord créer un arbre.", WARNING));
            ExitDeleteTreeMenu := True;
        end if;

        while not ExitDeleteTreeMenu loop
            if ShowDeleteTreeMenu then
                New_Line;
                Put_Line ("--- Supprimer un arbre ---");
                New_Line;
                ShowExistingTrees;
                Put_Line (getExistingTreesLength'Image & ". Retourner au menu principal");
                New_Line;
                Put ("Entrez votre choix " & ChoicesIndex & ": ");
            end if;
            declare
                TreeToDeleteIndex : Integer;
            begin
                Get (Item => TreeToDeleteIndex);
                if TreeToDeleteIndex < 0 or TreeToDeleteIndex > getExistingTreesLength then
                    raise Data_Error;
                else
                    if not (TreeToDeleteIndex = getExistingTreesLength) then
                        New_Line;
                        Put_Line(getColoredString("L'arbre '" & To_String(ExistingTrees.Element(TreeToDeleteIndex - 1).Name) & "' a été supprimé.", SUCCESS)); 
                        Delete(ExistingTrees, TreeToDeleteIndex - 1);
                        -- TODO : delete memory from this tree ( stephane ? )
                    end if;
                end if;
                ExitDeleteTreeMenu := True;
                ShowMainMenu := True;
            exception
                when Data_Error =>
                    Skip_Line;
                    New_Line;
                    Put(getColoredString("Choix invalide, veuillez réessayer " & ChoicesIndex & ": ", ERROR));
                    ShowDeleteTreeMenu := False;
            end;
        end loop;
    end HandleDeleteTree;

    procedure HandleChangeVerbosity is
        ExitChangeVerbosityMenu : Boolean := False;
        ShowVerbositySettings : Boolean := True;
    begin
        while not ExitChangeVerbosityMenu loop
            if ShowVerbositySettings then
                Put_Line ("--- Verbosité ---");
                New_Line;
                Put_Line("Verbosité actuellement définie:" & Integer'Image (Verbosity));
                New_Line;
                Put_Line ("1. Afficher 'rôle' : 'clé'");
                Put_Line ("2. Afficher 'rôle' : 'nom et prénom'");
                Put_Line ("3. Afficher toutes les informations connues");
                Put_Line ("4. Afficher toutes les informations");
                Put_Line ("5. Retourner au menu principal");
                New_Line;
                Put ("Entrez votre choix (1-5): ");
            end if;
            declare
                NewVerbosity : Integer;
            begin
                Get (Item => NewVerbosity);
                if NewVerbosity /= 5 then
                    Verbosity := NewVerbosity;
                    New_Line;
                    Put_Line (getColoredString("La verbosité a été définie à" & Integer'Image(Verbosity), SUCCESS));
                end if;
                ExitChangeVerbosityMenu := True;
                ShowMainMenu := True;
            exception
                when Data_Error =>
                    Skip_Line;
                    New_Line;
                    Put(getColoredString("Choix invalide, veuillez réessayer (1-5): ", ERROR));
                    ShowVerbositySettings := False;
            end;
        end loop;
    end HandleChangeVerbosity;

begin
    Put_Line(Integer(ExistingTrees.Length)'Image);
    AddDefaultTree;

    HandleCreateTree(CurrentTree);
    
    Put_Line(Integer(ExistingTrees.Length)'Image);

    for el of ExistingTrees loop
        showFamilyTree (el.Tree);
    end loop;
    
    Put_Line(Integer(ExistingTrees.Length)'Image);

    while ExitMainMenu loop
        if ShowMainMenu then
            New_Line;
            Put_Line ("--- Menu principal ---");
            New_Line;
            Put_Line ("1. Choisir un arbre");
            Put_Line ("2. Créer un arbre");
            Put_Line ("3. Supprimer un arbre");
            Put_Line ("4. Changer la verbosité (" & Trim(Verbosity'Image, Ada.Strings.Left) & " actuellement)");
            Put_Line ("5. Quitter");
            New_Line;
            Put ("Entrez votre choix (1-5): ");
        end if;
        begin
            Get (Item => MainMenuChoice);
        exception
            when Data_Error =>
                Skip_Line;
                MainMenuChoice := 0;
                ShowMainMenu := False;
        end;

        case MainMenuChoice is
            when 1 =>
                ShowMainMenu := True;
                HandleChooseTree(CurrentTree);
            when 2 =>
                ShowMainMenu := True;
                HandleCreateTree(CurrentTree);
            when 3 =>
                ShowMainMenu := True;
                HandleDeleteTree;
            when 4 =>
                ShowMainMenu := True;
                HandleChangeVerbosity;
            when 5 =>
                ExitMainMenu := True;
            when others =>
                Put(getColoredString("Choix invalide, veuillez réessayer (1-5): ", ERROR));
                ShowMainMenu := False;
        end case;
    end loop;
end Menu;
