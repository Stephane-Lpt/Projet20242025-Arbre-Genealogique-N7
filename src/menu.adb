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
    QuitCharacter  : constant Unbounded_String := To_Unbounded_String ("q");

    type String_Access is Access all Unbounded_String;
    type Boolean_Access is Access all Boolean;
    type Person_Access is Access all T_Person;

    OperationAbandonnedException : exception;

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

        Choice : Integer;
        ExitTreeMenu : Boolean := False;

    begin
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
                when 11 =>
                    ExitTreeMenu := True;
                when others =>
                    Put_Line ("Choix invalide, veuillez réessayer (1-11).");
            end case;
        end loop;

    end TreeMenu;
    
    procedure HandleInput(Pointer : in String_Access; Stop : in Boolean_Access; TextString : in String; InputType : in T_InputType := STR; MaxInt : in Integer := -1) is
        -- GET IMPUT FROM USER
        -- Pointer : pointer to the string that is going to be modified
        -- Stop : pointer to the stop flag (in case of 'q')
        -- TextString : Input prompt string 
        -- InputType : Expected input type from user (either string or integer)
        -- MaxInt : Maximum int that can be entered by user
        ExitInput : Boolean := False;
        ShowTextString : Boolean := True;
        Input : Unbounded_String;
        TempInt : Integer;
    begin
        while not ExitInput loop
            begin
                if ShowTextString then
                    Put (TextString);
                end if;

                Input := To_Unbounded_String(Get_Line);

                if Input = QuitCharacter then
                    ExitInput := True;
                    Stop.all := True;
                else
                    if InputType = INT then
                        --  IN CASE WHEN ASKED FOR AN INTEGER, CHECKING IF IT'S A VALID INT
                        TempInt := Integer'Value(To_String(Input));
                    end if;

                    -- ALSO CHECKING IF IT FALLS IN THE 1 .. MaxInt RANGE (only if MaxInt isn't the default value, -1)
                    if MaxInt /= -1 and (TempInt <= 0 or TempInt > MaxInt) then
                        raise Data_Error;
                    end if;

                    Pointer.all := Input;
                    ExitInput := True;
                end if;
            exception
                when Data_Error | Constraint_Error =>
                    -- QUOICOUBEH;
                    New_Line;
                    Put(getColoredString("Saisie invalide, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ", ERROR));
                    ShowTextString := False;
            end;
        end loop;
    end HandleInput;

    function GetKeyByInput(mustBePresent : in Boolean; checkPresence : in Boolean := True) return Integer is
        -- checkPresence to check if key is absent / present
        -- isPresent if the key is supposed to be present
        -- not isPresent if the key is supposed to be absent
        ExitGetKey : Boolean := False;
        ShowGetKeyMenu : Boolean := True;
        IntegerKey : Integer;
        Key : Unbounded_String;

        procedure Handle_Exception(Message : in String) is
        begin
            -- QUOICOUBEH;
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
    end GetKeyByInput;

    function GetExistingTreesLength return Integer is
    begin
        return Integer(ExistingTrees.Length) + 1;
    end GetExistingTreesLength;

    procedure AddDefaultTree is
        Tuple : TreeTuple;
    begin
        Tuple.Name := To_Unbounded_String("Arbre exemple");
        Tuple.Tree := FamilyTree.GetExampleFamilyTree;
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
            Put_Line (getTrimmedInt(Index) & ". " & To_String(el.Name));
            Index := Index + 1;
        end loop;
    end ShowExistingTrees;

    ExitCreateNewPersonMenu : aliased Boolean := False;
    NewPerson : aliased T_Person;
    NewPersonKey : aliased Integer := 1;
    FirstName : aliased Unbounded_String;
    LastName : aliased Unbounded_String;
    Gender : aliased Unbounded_String;
    Birthdate : aliased Unbounded_String;
    procedure HandleCreateNewPerson(Position : in T_Position := ROOT) is
    begin
        New_Line;
        if Position = ROOT then
            Put_Line ("--- Ajoute de l'enfant ---");
        else
            Put_Line ("--- Ajout d'un nouvel ancêtre ---");
        end if;
        New_Line;

        --  NewPersonKey := GetKeyByInput(False, False);
        NewPersonKey := NewPersonKey + 1;
        --  if NewPersonKey = -1 then
        --      ExitCreateNewPersonMenu := True;
        --  end if;

        HandleInput (Pointer => FirstName'Access, Stop => ExitCreateNewPersonMenu'Access, TextString => "Entrez le prénom de la personne: ");
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

            if Position /= ROOT then
                New_Line;
                Put_Line(getColoredString("Un nouvel ancêtre a été ajoutée", SUCCESS));
            end if;
        else
            New_Line;
            if Position /= ROOT then
                Put_Line(getColoredString("Abandon de l'operation. L'ancêtre n'a pas été ajouté.", WARNING));
            else
                Put_Line(getColoredString("Abandon de l'operation. L'arbre n'a pas été crée.", WARNING));
            end if;

            raise OperationAbandonnedException;
        end if;
    end HandleCreateNewPerson;

    ExitTreeOperationMenu : aliased Boolean := False;
    TreeOperationIndex : aliased Unbounded_String;
    procedure HandleTreeOperation(Operation : in T_OperationType) is
        TitleText : String := (if Operation = CHOOSE then "Choisir un arbre" else "Supprimer un arbre");
    begin
        if GetExistingTreesLength - 1 > 0 then
            New_Line;
            Put_Line ("--- " & TitleText & " ---");
            New_Line;
            ShowExistingTrees;
            Put_Line ("q. Retourner au menu principal");
            New_Line;

            TreeOperationIndex := To_Unbounded_String("-1");
            HandleInput (Pointer => TreeOperationIndex'Access, Stop => ExitTreeOperationMenu'Access, TextString => "Entrez votre choix " & getMenuRangeString (GetExistingTreesLength) & ": ", InputType => INT, MaxInt => GetExistingTreesLength - 1);

            if not ExitTreeOperationMenu then
                if Operation = CHOOSE then
                    CurrentTree := ExistingTrees.Element (Integer'Value(To_String(TreeOperationIndex)) - 1);

                    New_Line;
                    Put_Line(getColoredString("L'arbre '" & To_String(CurrentTree.Name) & "' a été choisi.", SUCCESS)); 
                    TreeMenu;
                elsif OPERATION = DELETE then
                    --  TODO : delete memory from this tree ( stephane ? )

                    New_Line;
                    Put_Line(getColoredString("L'arbre '" & To_String(ExistingTrees.Element(Integer'Value(To_String(TreeOperationIndex)) - 1).Name) & "' a été supprimé.", SUCCESS));
                    Delete(ExistingTrees, Integer'Value(To_String(TreeOperationIndex)) - 1);
                end if;
            end if;
        else
            New_Line;
            Put_Line(getColoredString("Aucun arbre existant. Veuillez d'abord en créer un.", WARNING));
        end if;
    end HandleTreeOperation;

    ExitCreateNewTreeMenu : aliased Boolean := False;
    NewTreeName : aliased Unbounded_String;
    procedure HandleCreateTree is
        Tree: T_FamilyTree renames CurrentTree.Tree;
    begin
        New_Line;
        Put_Line ("--- Créer un arbre ---");
        New_Line;

        HandleInput (Pointer => NewTreeName'Access, Stop => ExitCreateNewTreeMenu'Access, TextString => "Entrez le nom de l'arbre à créer ('q' pour quitter): ");
        
        if not ExitCreateNewTreeMenu then
            AddNewTree (To_String(NewTreeName));

            begin
                HandleCreateNewPerson;

                CurrentTree := ExistingTrees.Element(Index => ExistingTrees.Last_Index);
                initChild (Tree, NewPersonKey, NewPerson);
                ExistingTrees.Replace_Element(ExistingTrees.Last_Index, CurrentTree);
                New_Line;
                Put_Line(getColoredString("L'arbre " & To_String(CurrentTree.Name) & " a été crée.", SUCCESS));
            exception
                when OperationAbandonnedException =>
                    Null;
            end;
        else
            New_Line;
            Put_Line(getColoredString("Abandon de l'operation. L'arbre n'a pas été crée.", WARNING));
        end if;
    end HandleCreateTree;
	
    NewVerbosity : aliased Unbounded_String;
    ExitChangeVerbosityMenu : aliased Boolean := False;
    procedure HandleChangeVerbosity is
    begin
        New_Line;
        Put_Line ("--- Verbosité ---");
        New_Line;
        Put_Line("Verbosité actuellement définie:" & Integer'Image (Verbosity));
        New_Line;
        Put_Line ("1. Afficher 'rôle' : 'clé'");
        Put_Line ("2. Afficher 'rôle' : 'nom et prénom'");
        Put_Line ("3. Afficher toutes les informations connues");
        Put_Line ("4. Afficher toutes les informations");
        Put_Line ("q. Retourner au menu principal");
        New_Line;

	    NewVerbosity := To_Unbounded_String("-1");
        HandleInput (Pointer => NewVerbosity'Access, Stop => ExitChangeVerbosityMenu'Access, TextString => "Entrez votre choix " & getMenuRangeString (4) & ": ", InputType => INT, MaxInt => 4);

        if not ExitChangeVerbosityMenu then
            if Integer'Value(To_String(NewVerbosity)) /= -1 then
                Verbosity := Integer'Value(To_String(NewVerbosity));
                New_Line;
                Put_Line (getColoredString("La verbosité a été définie à" & Integer'Image(Verbosity), SUCCESS));
            end if;
        end if;
    end HandleChangeVerbosity;

    MainMenuChoice : aliased Unbounded_String;
    ExitMainMenu : aliased Boolean := False;
begin
    AddDefaultTree;

    while not ExitMainMenu loop
        New_Line;
        Put_Line ("--- Menu principal ---");
        New_Line;
        Put_Line ("1. Choisir un arbre");
        Put_Line ("2. Créer un arbre");
        Put_Line ("3. Supprimer un arbre");
        Put_Line ("4. Changer la verbosité (" & getTrimmedInt(Verbosity) & " actuellement)");
        Put_Line ("q. Quitter");
        New_Line;

        HandleInput (Pointer => MainMenuChoice'Access, Stop => ExitMainMenu'Access, TextString => "Entrez votre choix " & getMenuRangeString (4) & ": ", InputType => INT, MaxInt => 4);

        if not ExitMainMenu then
            case Integer'Value(To_String(MainMenuChoice)) is
                when 1 =>
                    HandleTreeOperation (CHOOSE);
                when 2 =>
                    HandleCreateTree;
                when 3 =>
                    HandleTreeOperation (DELETE);
                when 4 =>
                    HandleChangeVerbosity;
                when others =>
                    Null;
            end case;
        end if;
    end loop;
end Menu;
