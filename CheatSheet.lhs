\documentclass[11pt]{article}
%include lhs2TeX.fmt
\usepackage[T1]{fontenc}
\usepackage[sc]{mathpazo}
\linespread{1.05}
\usepackage{helvet}

\usepackage{multicol}
\usepackage{float}
\usepackage[landscape, top=0.2in, bottom=1in, left=0.2in, right=0.2in, dvips]{geometry}
\usepackage{verbatim}
\usepackage{fancyhdr}
\usepackage{paralist}

\usepackage{hyperref}
\usepackage[all]{hypcap} % Must be after hyperref
% \usepackage{setspace}
\hypersetup{colorlinks}

\pagestyle{fancy}
\fancyhf{}
\lfoot{\copyright\ 2010 Justin Bailey.}
\cfoot{\thepage}
\rfoot{\href{mailto:jgbailey@@codeslower.com}{\tt jgbailey@@codeslower.com}}
\renewcommand\footrulewidth{0.4pt}

\makeatletter
% Copied from article.cls; second-to-last parameter changed to -\parindent.
\renewcommand\subsubsection{\@@startsection{subsubsection}{3}{\z@@}%
  {-3.25ex \@@plus -1ex \@@minus -.2ex}%
  {-\parindent}%
  {\normalfont\normalsize\bfseries}}
\makeatother

\newcommand{\hd}[1]{\section*{\textsf{#1}}}
\newcommand{\shd}[1]{\subsection*{\textsf{#1}}}
\newcommand{\sshd}[1]{\subsubsection*{\textsf{#1}}}
\setlength{\columnsep}{18.0pt}
\setlength{\columnseprule}{0.4pt}
\begin{document}
% \doublespacing
\begin{multicols}{3}
\section*{\textsf{\LARGE Haskell Cheat Sheet\normalsize}}\label{preamble}

Cette "antisèche" couvre les éléments fondamentaux du langage Haskell:
syntaxe, mots réservés et autres éléments. Elle est présentée à la fois
comme un fichier Haskell exécutable et un document imprimable. Chargez
le code source dans votre interpréteur pour tester les exemples inclus.

\begin{comment}

> {-# LANGUAGE MultiParamTypeClasses,NPlusKPatterns, DatatypeContexts #-}
>
> module CheatSheet where
>
> import Data.Char (isUpper, isLower, toUpper, toLower, isSpace, GeneralCategory(..))
> import System.IO (readFile)
> import System.Directory (doesFileExist)
> import qualified Data.Set as Set
> import qualified Data.Char as Char

\end{comment}

\hd{Syntaxe de base}\label{syntax}

\shd{Commentaires}\label{comments}

  Un commentaire de ligne commence par `@--@' et s'étends jusqu'à la fin de la ligne.
  Les commentaires Multi-lignes commencent par '@{-@' et s'étendent jusqu'au '@-}@'. Les
  commentaires peuvent être imbriqués.

  Les commentaires situés juste au dessus des définitions des fonctions doivent commencer par `@{- |@'
  et les types de paramètres par `@-- ^@' pour des raisons de compatibilité avec Haddock, un système
  de documentation du code Haskell.

\shd{Mots réservés}\label{reserved-words}

  Les mots suivants sont réservés en Haskell. Nommer une variable ou une fonction avec
  l'un de ces noms constitue une erreur de syntaxe.

  \setlength{\columnsep}{10.0pt}
  \setlength{\columnseprule}{0.0pt}
  \begin{multicols}{3}
    \begin{compactitem}
      \item @case@
      \item @class@
      \item @data@
      \item @deriving@
      \item @do@
      \item @else@
      \item @if@
      \item @import@
      \item @in@
      \item @infix@
      \item @infixl@
      \item @infixr@
      \item @instance@
      \item @let@
      \item @of@
      \item @module@
      \item @newtype@
      \item @then@
      \item @type@
      \item @where@
    \end{compactitem}
  \end{multicols}
  \setlength{\columnsep}{18.0pt}
  \setlength{\columnseprule}{0.4pt}

\shd{Chaînes de caractères}\label{strings}

  \begin{compactitem}
  \item @"abc"@ -- Chaîne Unicode, équivalent à @['a','b','c']@.
  \item @'a'@ -- Un caractère.
  \end{compactitem}

  \sshd{Chaînes de caractères Multi-lignes}\label{multi-line-strings}

  Normalement, une chaîne de caractères ne doit pas contenir de saut de ligne. Cela
  constitue une erreur de syntaxe.

  Donc, ceci est une erreur de syntaxe:

< string1 = "My chaîne
< longue."

  Un caractère "barre inverse" (`@\@') permet d'``échapper'' un saut de ligne:

> string1 = "Ma chaîne \
> \longue."

  La zone entre les caractères "barre inverse" est ignorée. Les sauts de lignes \emph{dans} la chaîne
  doivent être représentés explicitement:

> string2 = "Ma chaîne \n\
> \longue."

  Donc, @string1@ vaut:

< Ma chaîne longue.

  Alors que @string2@ vaut:

< Ma chaîne
< longue.

  \sshd{Codes d'échappement} Les caractères d'échappement suivants peuvent être utilisés pour les caractères ou les chaînes de caractères:
  \begin{compactitem}
    \item @\n@, @\r@, @\f@, etc. -- Les codes standards pour saut de ligne, retour chariot, saut de page, etc. sont supportés.
    \item @\72@, @\x48@, @\o110@ -- Un caractère de valeur 72 en
      décimal, hexa et octal, respectivement.
    \item @\&@ -- Un caractère d'échappement ``nul'' qui autorise la présence de codes d'échappement au côté de caractères numériques litéraux. Par exemple, @\x2C4@ est un
      $\wedge$ (en Unicode) alors que @\x2C\&4@ représente @,4@. Cette séquence ne peut pas être utilisé en tant que
      caractères litéraux.
  \end{compactitem}


\shd{Nombres}\label{numbers}

  \begin{compactitem}
  \item @1@ -- Valeur flottante ou entière.
  \item @1.0, 1e10@ -- Valeur flottante.
  \item @0o1, 0O1@ -- Valeur Octale.
  \item @0x1, 0X1@ -- Valeur Hexadécimale.
 \item  @-1@ -- Nombre négatif; le signe moins (``@-@'') ne peut pas être séparé du nombre.
  \end{compactitem}

\shd{Enumérations}\label{enumerations}

  \begin{compactitem}
  \item @[1..10]@ -- Liste de nombre -- \texttt{1, 2, {\ensuremath\mathellipsis}, 10}.
  \item @[100..]@ -- Liste de nombre infinie -- \texttt{100, 101, 102, {\ensuremath\mathellipsis}\ }.
  \item @[110..100]@ -- Liste vide, mais @[110, 109 .. 100]@ donnera une liste de 110 à 100.
  \item @[0, -1 ..]@ -- Nombres négatifs.
  \item @[-110..-100]@ -- Erreur de syntaxe; il faut remplacer par @[-110.. -100]@ pour les négatifs (espace supplémentaire).
  \item @[1,3..99], [-1,3..99]@ -- Liste de 1 à 99, par incréments de 2, de -1 à 99 par incréments de 4.
  \end{compactitem}

  \noindent De fait, n'importe quelle valeur appartenant à la classe @Enum@ peut être utilisée:

  \begin{compactitem}
  \item @['a' .. 'z']@ -- Liste de caractères -- \texttt{a, b, {\ensuremath\mathellipsis}, z}.
  \item @['z', 'y' .. 'a']@ -- \texttt{z, y, x, {\ensuremath\mathellipsis}, a}.
  \item @[1.0, 1.5 .. 2]@ -- @[1.0,1.5,2.0]@.
  \item @[UppercaseLetter ..]@ -- Liste de valeurs @GeneralCategory@ (de @Data.Char@).
  \end{compactitem}

\shd{Listes \& Tuplets}\label{lists-tuples}

  \begin{compactitem}
  \item @[]@ -- Liste vide.
  \item @[1,2,3]@ -- Liste de trois nombres.
  \item @1 : 2 : 3 : []@ -- Alternative pour construire une liste, en utilisant ``cons'' (@:@) et ``nil'' (@[]@).
  \item @"abc"@ -- Liste de trois caractères (les chaînes de caractères sont des listes).
  \item @'a' : 'b' : 'c' : []@ -- Liste de caractères (équivalent à @"abc"@).
  \item @(1,"a")@ -- tuplet de 2-éléments (nombre et chaîne de caractères).
  \item @(head, tail, 3, 'a')@ -- tuplet de 4-éléments constitué de deux fonctions, d'un nombre et d'un caractère.
  \end{compactitem}

\shd{règle de ``Mise en forme'', accolades, point-virgules.}\label{layout}

 Haskell supporte les accolades et les point-virgules, comme en C. Cependant
 personne ne les utilise. A la place, on utilise des règles de ``mise en forme'',
 avec lesquelles l'indentation représente la portée.
 La règle générale est la suivante: toujours indenter. Quand le compilateur se plaint, indenter plus.

  \sshd{Accolates et point-virgules}\label{braces-semicolons}

  Les point-virgules terminent une expression, tandis que les accolates représentent la portée. Ils sont
  utilisables avec les mots réservés suivants: @where@, @let@, @do@ and @of@. Ils ne le sont pas lors de
  la définition du corps d'une fonction. Par exemple, le code suivant ne compilera pas:

<    square2 x = { x * x; }

  Cependant, celui-ci fonctionnera sans soucis:

> square2 x = result
>     where { result = x * x; }

  \sshd{Définition de fonction}\label{layout-function-definition}

  Indenter le corps d'au moins un espace à partir du nom de la fonction:

< square x  =
<   x * x

  Sauf si une clause @where@ est présente. Dans ce cas, indenter la clause where d'au moins un espace
  à partir du nom de la fonction. Indenter tout corps de fonction d'au moins un espace à partir du mot
  réservé @where@:

<  square x =
<      x2
<    where x2 =
<      x * x

  \sshd{Let}\label{layout-let}

  Indenter le corps d'au moins un espace depuis la première définition du @let@. Si @let@ apparaît sur sa propre ligne, alors le corps doit apparaître dans la colonne après le @let@:

<  square x =
<    let x2 =
<          x * x
<    in x2

  Comme on peut le voir ci-dessus, le mot réservé @in@ doit aussi être dans la même colonne que le @let@. Enfin, quand
  plusieurs définitions sont données, tout les identifiants doivent apparaître dans la même colonne.

\hd{Déclarations, Etc.}\label{declarations}

  La partie suivante détaille les règles de déclaration des fonctions, les listes "en place" TODO:comprehensions, et d'autres
  parties du langage.

\shd{Définition de fonction}\label{function-definition}

  Les fonctions sont définies en déclarant son nom, ses arguments et un signe "égal":

> square x = x * x

  \emph{Tout} les noms de fonctions doivent débuter par une lettre minuscule ou le caractère ``@_@''. Autrement, il s'agit d'une erreur de syntaxe.

  \sshd{Motifs de correspondance}\label{pattern-matching}

  Plusieurs ``clauses'' d'une fonction peuvent être définies avec le mécanismede ``motifs de correspondance'' utilisé sur
  les valeurs des arguments. Ici, la fonction @agree@ a 4 cas distincts:

> -- Détecte quand la chaîne "y" est trouvée.
> agree1 "y" = "Super!"
> -- Détecte quand la chaîne "n" est trouvée.
> agree1 "n" = "Pas de chance."
> -- Détecte la correspondance du début d'une chaîne
> -- avec 'y'.
> agree1 ('y':_) = "YAHOO!"
> -- Accepte toute autre valeur.
> agree1 _ = "Tellement triste."

  Notez que le caractère`@_@' est un méta-caractère et intercepte n'importe quelle valeur.

  Les motifs de correspondance peuvent être utilisés de manière imbriquée. Prenons cette déclaration:

< data Bar = Bil (Maybe Int) | Baz

  \noindent en gardant à l'esprit la \hyperref[maybe]{définition de @Maybe@}
  page~\pageref{maybe}, on peut trouver des correspondances de valeurs sur @Maybe@ imbriquées quand @Bil@ est
  présent:TODO:Vérifier sens et sémantique

< f (Bil (Just _)) = ...
< f (Bil Nothing) = ...
< f Baz = ...

  Les motifs de correspondance permettent également d'affecter des valeurs à des variables. Par exemple,
  cette fonction détermine si la chaîne donnée est vide ou non. Si ce n'est pas le cas, alors la valeur
  liée à @str@ est convertie en minuscules:

> toLowerStr [] = []
> toLowerStr str = map toLower str

  Notez que @str@ ci-dessus est similaire à @_@ car il intercepte tout; la seule
  différence est que la valeur détectée se voit donnée un nom.

  \sshd{{\ensuremath $n + k$} Patterns}\label{plus-patterns}

  Ce mécanisme de motifs de correspondance (parfois controversé) facilite la mise en correspondance
  de certains types d'expressions numériques. L'idée est de définir un cas de base (la partie
  ``$n$'') avec un nombre constant pour la correspondance, et ensuite de définir d'autres correspondances
  (la partie ``$k$'') comme des additions au cas de base. Voici une façon plutôt inefficace de vérifier
  si un nombre est pair ou non:

> isEven 0 = True
> isEven 1 = False
> isEven (n + 2) = isEven n

  \sshd{Capture des arguments}\label{argument-capture}

  La capture d'argument est utile pour détecter une valeur \emph{et} l'utiliser,
  sans devoir déclarer de variable supplémentaire. Utiliser un symbole `|@|' entre le motif
  à détecter et la variable pour lui affecter une valeur. Ce mécanisme est utilisé ci-dessous
  pour affecter la tête de la liste à @l@ afin de l'afficher, tout en affectant également
  l'ensemble de la liste à @ls@ dans le but de calculer sa longueur:

> len ls@(l:_) = "La liste commence par " ++
>   show l ++ " et est longue de " ++
>   show (length ls) ++ " éléments."
> len [] = "La liste est vide!"

  \sshd{Conditions d'entrées}\label{function-guards}

  Les fonctions booléennes peuvent être utilisées comme des ``conditions d'entrée'' dans les définitions de fonction
  grâce notamment aux motifs de correspondance. Un exemple sans motif de correspondance:

> which n
>   | n == 0 = "zero!"
>   | even n = "pair!"
>   | otherwise = "impair!"

    Notez @otherwise@ -- qui vaut toujours to @True@ et peut être utilisé to spécifier un branchement par ``défaut''.

    Les conditions d'entrées peuvent être utilisées avec les motifs. Ici une fonction qui détermine
    si le premier caractère d'une chaîne est une majuscule ou une minuscule:

> what [] = "chaîne vide!"
> what (c:_)
>   | isUpper c = "majuscule!"
>   | isLower c = "minuscule!"
>   | otherwise = "pas une lettre!"

  \sshd{Correspondance \& ordre des conditions d'entrées}\label{function-matching-order}

  Les motifs de correspondance s'évaluent dans l'ordre de haut en bas. De la même façon, les
  conditions d'entrée sont testées de haut en bas. Par exemple, aucune de ces fonctions ne serait
  très intéréssante:

> allEmpty _ = False
> allEmpty [] = True
>
> alwaysEven n
>   | otherwise = False
>   | n `div` 2 == 0 = True

  \sshd{Syntaxe des enregistrements}\label{matching-record-syntax}

  Normally pattern matching occurs based on the position of arguments in the
  value being matched. Types declared with record syntax, however, can match
  based on those record names. Given this data type:

> data Color = C { red
>   , green
>   , blue :: Int }

\begin{comment}

>   deriving (Show, Eq)

\end{comment}

  \noindent Une seule correspondance possible sur une occurence de @green@:

> isGreenZero (C { green = 0 }) = True
> isGreenZero _ = False

  La capture d'argument est possible avec cette syntaxe, même si çà peut devenir bizarre.
  Nous pouvons définir un type @Pixel@ et une fonction pour remplacer des valeurs avec des composantes @green@ différente de zéro avec tout en noir @TODO:Vérifier:

> data Pixel = P Color

\begin{comment}

>   deriving (Show, Eq)

\end{comment}

> -- Valeur de la couleur non altéré si green est égal à zéro
> setGreen (P col@(C { green = 0 })) = P col
> setGreen _ = P (C 0 0 0)

  \sshd{Motifs fainéants}\label{lazy-patterns}

  Cette syntaxe, également connue comme motifs \emph{irréfutables}, autorise des motifs
  qui seront toujours détectés. Ceci implique que toute clause correspondra mais si l'on
  essaie d'utiliser la valeur, alors une erreur peut se produire. C'est généralement utile
  quand on doit déclencher une action pour un \emph{type} et une valeur précise y compris
  si cette valeur n'existe pas.

  Par exemple, on peut définir une classe avec des valeurs par défaut:

> class Def a where
>   defValue :: a -> a

  Le principe est de donner à @defValue@ une valeur du bon type ce qui retournera
  une valeur par défaut pour ce type. Définir des instances pour les types de base
  est simple:

> instance Def Bool where
>   defValue _ = False
>
> instance Def Char where
>   defValue _ = ' '

  @Maybe@ est un peu plus subtil, car on souhaite obtenir une valeur par défaut pour le type,
  alors que le constructeur peut être @Nothing@. La définition suivante marcherait, mais ce
  n'est pas optimal car on obtient @Nothing@ dés lors que @Nothing@ est passé en entrée.

< instance Def a => Def (Maybe a) where
<   defValue (Just x) = Just (defValue x)
<   defValue Nothing = Nothing

  Il vaudrait mieux obtenir un {\tt Just (\rm\emph{default value}\tt)\rm} à la place.
  Voici comment un "motif fainéant" nous sauve -- On peut faire comme ci on avait
  détecté @Just x@ et utiliser ceci pour avoir une valeur par défaut, même si @Nothing@ est donné:

> instance Def a => Def (Maybe a) where
>   defValue ~(Just x) = Just (defValue x)

  Tant que la valeur @x@ n'a pas encore été évaluée, tout va bien. Aucun des types de base
  ne nécessite de voir @x@ (voir la correspondance ``@_@'' qu'ils utilisent), donc cela
  fonctionnera sans problème.

  Un souci avec ce qui précède provient du fait que l'on doit annoter les types
  au niveau de l'interpréteur ou du code quand on utilise un constructeur @Nothing@. @Nothing@ a
  le type @Maybe a@ mais, si il n'y a pas suffisamment d'information disponible, on doit indiquer
  à Haskell le type correspondant à @a@. Quelques exemples de valeurs par défaut:

> -- Return "Just False"
> defMB = defValue (Nothing :: Maybe Bool)
> -- Return "Just ' '"
> defMC = defValue (Nothing :: Maybe Char)

\shd{Constructions de Listes}\label{list-comprehensions}

  Une construction de liste consiste en quatre types d'éléments: des \emph{générateurs},
  des \emph{conditions d'entrée}, \emph{des affectations locales}, et des \emph{cibles}. Une construction
  de liste créé une liste de valeurs cibles à partir des générateurs et conditions d'entrées spécifiées.

  Cette construction génère tous les carrés:

> squares = [x * x | x <- [1..]]

  @x <- [1..]@ génère une liste de toutes les valeurs entières @Integer@ et les affecte à @x@,
  une par une. @x * x@ créé chaque élément de la liste en multipliant @x@ avec lui même.

  Les conditions permettent d'exclure certains éléments. Ce qui suit montre comment les diviseurs
   d'un nombre donné (excluant ce nombre) peuvent être calculés. Notez comment @d@ est utilisé à la
   fois dans la condition d'entrée et dans l'expression cible.

> divisors n =
>   [d | d <- [1..(n `div` 2)]
>      , n `mod` d == 0]

  Les affectations locales fournissent de nouvelles définitions pouvant être utilisées dans l'expression
  générée ou par les générateurs et conditions d'entrées déclarés à partir de là. Ci-dessous, @z@ est utilisé pour
  représenter le minimum de @a@ et @b@:

> strange = [(a,z) | a <-[1..3]
>                  , b <-[1..3]
>                  , c <- [1..3]
>                  , let z = min a b
>                  , z < c ]

  Les constructions ne sont pas limitées aux nombres. N'importe quelle liste fera l'affaire. L'ensemble des
  lettres majuscules peut être généré ainsi:

> ups =
>   [c | c <- [minBound .. maxBound]
>      , isUpper c]

  Ou bien, pour trouver toutes les occurrences d'une valeur d'arrêt particulière @br@ dans une liste de @word@
  (indexée à partir de 0):

> idxs word br =
>   [i | (i, c) <- zip [0..] word
>       , c == br]

  Une fonctionnalité unique des constructions de liste est que les erreurs liées aux motifs
  de correspondance ne sont pas prises en compte; elles sont simplements exclues de la liste
  des résultats.

\shd{Operators}\label{operators}

  Il y a très peu d'``opérateurs'' prédéfinis en Haskell.  --- la plupart d'entre eux relèvent
  en fait de la syntaxe (ex: ``@=@''). A la place, ces opérateurs sont simplement des fonctions qui prennent
  deux arguments et qui ont un support syntaxique particulier.

  De tels opérateurs peuvent être appliqués comme fonction préfixée à condition de les entourer
  de parenthèses:

< 3 + 4 == (+) 3 4

  Pour définir un nouvel opérateur, il faut simplement l'implémenter comme une fonction normale, sauf que
  l'opérateur apparaît entre les deux arguments dans la signature. En voici un qui insère une virgule entre deux chaînes et s'assure qu'aucun espace supplémentaire n'apparaît:

> first ## last =
>   let trim s = dropWhile isSpace
>         (reverse (dropWhile isSpace
>           (reverse s)))
>   in trim last ++ ", " ++ trim first

< > "  Haskell " ## " Curry "
< Curry, Haskell

  Bien sûr, les motifs de correspondance et conditions d'entrées, etc sont disponibles sous cette forme.
  Les signatures de Type sont un peu différentes il est vrai. Le ``nom'' de l'opérateur doit apparaître entouré
  de parenthèses:

> (##) :: String -> String -> String

  Les symboles autorisés pour définir des opérateurs sont les suivants:

< # $ % & * + . / < = > ? @ \ ^ | - ~

  Cependant, il y a plusieurs ``opérateurs'' qui ne peuvent pas être rédéfinis. Ce sont:
  @<-@, @->@ and @=@. Le dernier, @=@, ne peut pas être redéfini seul, mais peut faire partie
  d'un opérateur multi-caractères. La fonction ``bind'', @>>=@ en est un exemple.

  \sshd{Précédence \& Associativité}\label{fixity}

  La précédence et l'associativité, appelés collectivement \emph{fixité}, de n'importe quel
  opérateur peut être affectée grâce aux mots réservés @infix@, @infixr@ et @infixl@. Ceux-ci
  peuvent être appliqués à la fois aux fonctions de plus haut niveau et aux définitions locales.
 La syntaxe est la suivante:

\bigskip
  \textbraceleft\texttt{infix} || \texttt{infixr} || \texttt{infixl}\textbraceright\ \emph{precedence op}
\bigskip

  \noindent où \emph{precedence} varie de 0 à 9. \emph{Op} peut être de fait
  n'importe quelle fonction prenant deux arguments (càd, n'immporte quelle opération binaire).
  Le fait que l'opérateur soit associatif à gauche ou à droite est spécifié par @infixl@ ou
  @infixr@, respectivement. De telles déclarations @infix@ n'ont pas d'associativité.

  La Précédence et l'associativité permettent de faire fonctionner toutes les règles de l'arithmétique ``tel qu'attendu.''
  Par exemple, considérez les mises à jour mineures de précédence entre addition et multiplication:

> infixl 8 `plus1`
> plus1 a b = a + b
> infixl 7 `mult1`
> mult1 a b = a * b

  Les résultats sont surprenants:

< > 2 + 3 * 5
< 17
< > 2 `plus1` 3 `mult1` 5
< 25

  L'associativité inverse n'a pas d'effets intéressants. En redéfinissant la division pour la rendre associative
  à droite:

> infixr 7 `div1`
> div1 a b = a / b

  On obtient des résultats intéréssants:

< > 20 / 2 / 2
< 5.0
< > 20 `div1` 2 `div1` 2
< 20.0

\shd{Currying}\label{currying}

 En Haskell, les fonctions ne doivent pas forcément recevoir l'ensemble de leurs arguments en une seule fois. Par
 exemple, considérez la fonction @convertOnly@, qui convertit certains éléments en chaînes de caractères en fonction
 d'un test:

> convertOnly test change str =
>     map (\c -> if test c
>                 then change c
>                 else c) str

 En utilisant @convertOnly@, on peut écrire la fonction @l33t@ qui convertit certaines lettres en nombres:

> l33t = convertOnly isL33t toL33t
>   where
>     isL33t 'o' = True
>     isL33t 'a' = True
>     -- etc.
>     isL33t _ = False
>     toL33t 'o' = '0'
>     toL33t 'a' = '4'
>     -- etc.
>     toL33t c = c

 Notez que @l33t@ n'a pas d'arguments. Par ailleurs, le dernier argument à @convertOnly@ n'est pas donné. Cependant,
 la signature du type @l33t@ nous aide:

< l33t :: String -> String

 Donc, @l33t@ prends une chaîne et produit une chaîne. C'est une ``constante'',
 au sens que @l33t@ retourne toujours comme valeur une fonction prenant une chaîne et produisant une chaîne.
 @l33t@ retourne une forme ``curried'' de @convertOnly@, où seuls deux de ses trois arguments ont été fournis.

 Ceci peut être poussé plus avant. Disons que l'on souhaite écrire une fonction qui change seulement les lettres majuscules.
 Nous connaissons le test à appliquer (@isUpper@), mais on ne souhaite pas spécifier la conversion.
 Cette fonction peut être écrite comme suit:

> convertUpper = convertOnly isUpper

 qui a la signature de type:

< convertUpper :: (Char -> Char)
<   -> String -> String

 @convertUpper@ peut donc prendre deux arguments. Le premier est la fonction de conversion de
 caractères et la seconde correspond à la chaîne à convertir.

 Une forme curryfiée de la fonction multi-arguments peut être définie.
 Une façon de voir cela est que chaque ``flèche'' dans la signature de la fonction représente
 une nouvelle fonction qui peut être définie en lui fournissant un argument supplémentaire.

 \sshd{Sections}\label{sections}

 Les opérateurs sont des fonctions, et ils peuvent être currifiés comme n'importe quelle autre fonction. Par exemple, une
 version currifiée de ``@+@'' peut être écrite comme suit:

< add10 = (+) 10

 Cependant, cela peut être compliqué et difficile à lire. Les ``Sections'' sont des opérateurs currifiés, en utilisant
 des parenthèses. Voici la fonction @add10@ avec des sections:

> add10 = (10 +)

 L'argument fourni peut l'être à gauche ou à droite, ce qui indique la position qu'il doit prendre. C'est important
 pour des opérations comme la concaténation:

> onLeft str = (++ str)
> onRight str = (str ++)

 Ces fonctions produisent des résultats assez différents:

< > onLeft "foo" "bar"
< "barfoo"
< > onRight "foo" "bar"
< "foobar"

\shd{``Modifier'' des valeurs et syntaxe des enregistrements}\label{updating}

  Haskell est un langage pur et en tant que tel, il ne gère pas la modification d'état. Donc,
  une fois une valeur affectée, elle ne peut pas changer. La ``modification'' est en réalité
  une opération de copie, avec de nouvelles valeurs dans les champs ``modifiés.'' Par exemple,
  en utilisant le type @Color@ défini auparavant, on peut écrire une fonction qui positionne
  le champs @green@ à zéro facilement:

> noGreen1 (C r _ b) = C r 0 b

  Ceci est un peu verbeux et peut être réécrit en utilisant la syntaxe des enregistrements. Ce
  type de ``mise à jour'' positionne uniquement les champs spécifiés et copie le reste:

> noGreen2 c = c { green = 0 }

  Ici on capture la valeur de @Color@ dans @c@ et on retourne une nouvelle valeur de type @Color@.  Cette
  valeur @c@ aura la même valeur pour @red@ et @blue@ et sa composante @green@ à 0.
  On peut combiner ceci avec des motifs de correspondance afin d'affecter les champs
  @green@ et @blue@ fields de manière à être égal au champs @red@:

> makeGrey c@(C { red = r }) =
>   c { green = r, blue = r }

  Notez que l'on doit utiliser la capture d'argument (``|c@|'') pour accéder à la valeur de @Color@ et
  utiliser un motif de correspondance avec la syntaxe d'enregistrement (``|C { red = r}|'') afin d'obtenir
  le champs interne @red@.

\shd{Fonctions Anonymes}\label{anonymous-functions}

  Une fonction anonyme (càd, une \emph{expression lambda} ou encore \emph{lambda} en forme raccourcie), est une
  fonction sans nom. Elle peut être définie à n'importe quel endroit comme ici:

< \c -> (c, c)

  qui définit une fonction prenant un argument et retournant un tuplet
  contenant ledit argument aux deux emplacements. Elles sont utiles pour
  des fonctions simples ne nécessitant pas de nom. Ce qui suit détermine si
  une chaîne contient un mélange de lettres minuscules/majuscules ainsi que
  d'espaces.

> mixedCase str =
>   all (\c -> isSpace c ||
>              isLower c ||
>              isUpper c) str

  Bien sûr, les expressions lambdas peuvent être retournées également à partir de fonctions. Cet exemple classique
  retourne une fonction qui multipliera son argument par celui donné à l'origine:

> multBy n = \m -> n * m

  Par exemple:

< > let mult10 = multBy 10
< > mult10 10
< 100

\shd{Signatures}\label{type-signatures}

  Haskell implémente une inférence de type complète, ce qui veut dire que dans la
  plupart des cas, il n'est pas nécéssaire de préciser les types. Les signatures
  de type sont néanmoins toujours utiles pour au moins deux raisons:

  \begin{description}
  \item{\emph{Documentation}}---Même si le compilateur peut déterminer les types
  de vos fonctions, d'autres programmeurs ou vous  même pourriez ne  pas en être
  capables plus tard. Ecrire la signature (de type) sur toutes les fonctions de plus
  haut niveau est considérée comme une bonne pratique.

  \item{\emph{Spécialisation}}---Les types de classes (Typeclasses) permettent de surcharger *
   les fonctions.
  Par exemple, une fonction consistant à inverser le signe de n'importe quelle liste de nombres à la signature:

< negateAll :: Num a => [a] -> [a]

  Cependant, pour notamment des raisons d'efficacité, vous pourriez vouloir n'autoriser
  que les types @Int@. Vous pourriez accomplir ceci avec la signature de type:

< negateAll :: [Int] -> [Int]
  \end{description}

  Les signatures de Type peuvent apparaître pour les fonctions de plus haut niveau et les définitions @let@ ou @where@
  imbriquées. Généralement, ceci est utile pour la documentation, bien que dans certains cas, elles sont necéssaires
  pour empêcher le polymorphisme. Une signature de type est en premier lieu consituée du nom de l'élément à typer, suivi
  par un @::@, finalement suivi par le type. Un exemple de ceci a déjà été vu auparavant.

  Les signatures de Type n'ont pas besoin d'apparaître directement au dessus de leur implémentation.
  Ils peuvent être spécifiés n'importe où dans le module contenant (oui, même en dessous!).
  Plusieurs éléménts avec la même signature peuvent également être définis ensemble:

> pos, neg :: Int -> Int

< ...

> pos x | x < 0 = negate x
>       | otherwise = x
>
> neg y | y > 0 = negate y
>       | otherwise = y

  \sshd{Annotations de Type}\label{type-annotations}

  Parfois Haskell ne peut pas déterminer le type attendu. La démonstration classique de cela est le
  fameux problème du ``@show . read@'':

< canParseInt x = show (read x)

  Haskell ne peut pas compiler cette fonction car il ne connaît pas le type de @read x@.
  On doit affiner le type avec une annotation:

> canParseInt x = show (read x :: Int)

  Les Annotations ont la même syntaxe que les signatures de type, mais peuvent enrichir
  n'importe quelle expression. Remarquez que l'annotation ci-dessus est attachée à
  @read x@, et non pas sur la variable @x@. Seule l'application de la fonction (par exemple,
  @read x@) est plus forte que les annotations @TODO: Vérifier sémantique. If that was not the case,
  the above would need to be written @(read x) :: Int@.

\shd{Unit}\label{unit}

  @()@ -- Le type ``unit'' et la valeur ``unit''. La valeur et le type ne représentant aucune information utile.

\hd{Mots réservés}\label{keywords}

  Les mots réservés Haskell sont listés ci-dessous, par ordre alphabétique.

\shd{Case}\label{case}

  @case@ est similaire à l'instruction @switch@ en C\# ou Java, mais peut détecter un motif: La forme de la valeur
  inspectée.  Considérez un type de donnée:

> data Choices = First String | Second |
>   Third | Fourth

\begin{comment}

>   deriving (Show, Eq)

\end{comment}

  \noindent @case@ peut être utilisé pour déterminer le choix donné @TODO: Vérifier:

> whichChoice ch =
>   case ch of
>     First _ -> "1st!"
>     Second -> "2nd!"
>     _ -> "Something else."

  Comme avec les motifs de correspondance dans les définitions de fonction, le symbole ``méta-caractère'' `@_@' accepte
  n'importe quelle valeur.

  \sshd{Imbrication \& Capture}\label{nesting-capture}

  Les motifs et affectations imbriquées sont également autorisées. Par exemple, voici la définition du
  type @Maybe@ :

< data Maybe a = Just a | Nothing
\label{maybe}

  En utilisant @Maybe@ on peut déterminer si un choix a été donné à partir d'une "correspondance" imbriquée:

> anyChoice1 ch =
>   case ch of
>     Nothing -> "Pas de choix!"
>     Just (First _) -> "Premier!"
>     Just Second -> "Second!"
>     _ -> "Quelquechose d'autre."

  Les affectations peuvent être utilisées pour manipuler la valeur extraite:

> anyChoice2 ch =
>   case ch of
>     Nothing -> "Pas de choix!"
>     Just score@(First "gold") ->
>       "Premier avec l'or!"
>     Just score@(First _) ->
>       "Premier avec autre chose: "
>         ++ show score
>     _ -> "Non premier."

  \sshd{Ordre des correspondances}\label{case-matching-order}

  L'évaluation des correspondances se fait de haut en bas. If @anyChoice1@ est réordonné comme suite, alors le
  premier motif sera toujours détecté:

> anyChoice3 ch =
>   case ch of
>     _ -> "Quelquechose d'autre."
>     Nothing -> "Pas de choix!"
>     Just (First _) -> "Premier!"
>     Just Second -> "Second!"

  \sshd{Conditions d'entrées}\label{case-guards}

  Les gardes, ou conditions d'entrée, peuvent être utilisées de la même fonction que des
  définitions de fonctions. La seule différence tient dans l'utilisation de @->@ au lieu de @=@. Voici
  une fonction simple qui effectue une comparaison non sensible à la casse:

> strcmp s1 s2 = case (s1, s2) of
>   ([], []) -> True
>   (s1:ss1, s2:ss2)
>     | toUpper s1 == toUpper s2 ->
>         strcmp ss1 ss2
>     | otherwise -> False
>   _ -> False

\shd{Classe}\label{class}

  Une fonction Haskell est définie pour un type particulier ou un ensemble de types et ne
  peut être définie plus d'une fois. La plupart des langages supportent le mécanisme de
  ``surcharge'', où une fonction peut avoir différents comportements en lien avec le type
  de ses arguments. Haskell implémente la surcharge au travers de déclarations @classe@ et
  @instance@. Une @classe@ définit une ou plusieurs fonctions qui peuvent être appliquées
  à n'importe lequel des membres (ou instances)de cette classe. Une classe est comparable
  à une interface en Java ou C\#, et les instances à des implémentations concrètes de cette
  interface.

  Une classe doit être déclarée avec ou plusieurs variables de type. Techniquement, Haskell
  98 autorise seulement une variable de type, mais la plupart des implémentations d'Haskell
  supportent les \emph{classes de type multi-paramètres}, qui autorisent de fait plus d'une
  variable de type.

  On peut définir une classe qui fournit une variante pour un type donné:

> class Flavor a where
>   flavor :: a -> String

  Notez que la déclaration donne seulement la signature de type de la fonction---aucune
  implémentation n'est effectuée ici (avec néanmoins quelques exceptions, cf.
  \hyperref[defaults]{``Defaults''} en page~\pageref{defaults}). Pour poursuivre, on peut
  définir plusieurs instances:

> instance Flavor Bool where
>   flavor _ = "doux"
>
> instance Flavor Char where
>   flavor _ = "amer"

  L'évaluation de @flavor True@ donne:

< > flavor True
< "doux"

  While @flavor 'x'@ gives:

< > flavor 'x'
< "amer"

\sshd{Valeurs par défauts}\label{defaults}

  Des implémentations par défaut peuvent être données pour les fonctions d'une classe. Elles sont
  utiles quand certaines fonctions peuvent être définies à partir d'autres fonctions de la classe.

  Une valeur par défaut est définie en donnant un corps à une des fonctions membres. L'exemple
  canonique est @Eq@, qui définit @/=@ (non égal) en termes de of @==@\ :

< class Eq a where
<   (==) :: a -> a -> Bool
<   (/=) :: a -> a -> Bool
<   (/=) a b = not (a == b)

  Des définitions récursives peuvent être créées. Pour poursuivre l'exemple de @Eq@,
  @==@ peut être défini en termes de @/=@:

<   (==) a b = not (a /= b)

  Cependant, si les instances ne fournissent pas suffisamment d'implémentations concrètes des
  fonctions membres, alors un programme utilisant de telles instances bouclera.

\shd{Donnée}\label{data}

  Les fameux \emph{type de données algébriques} peuvent être déclarés comme suit:

> data MyType = MyValue1 | MyValue2

\begin{comment}

>   deriving (Show, Eq)

\end{comment}

  @MyType@ est le \emph{nom} du type. @MyValue1@ et @MyValue2@ sont des \emph{valeurs}
  du type et sont appelées \emph{constructeurs}. Plusieurs constructeurs peuvent être
  fournis en les séparant avec le caractère `@|@'. Remarquez que les noms du type
  et du constructeur \emph{doivent} débuter avec une lettre majuscule. Sinon,
  il s'agit d'une erreur de syntaxe.

  \sshd{Constructeurs avec des Arguments}\label{constructors-with-arguments}

  Le type ci-dessus n'est pas très intéressant sauf en tant qu'énumération. Les Constructeurs
  prenant des arguments peuvent être déclarés, autorisant le stockage de plus d'informations:

> data Point = TwoD Int Int
>   | ThreeD Int Int Int

  Remarquez que les arguments pour chacun des constructeurs sont des noms de \emph{type}, pas des
  constructeurs. Cela veut dire que ce type de déclaration est illégal:

< data Poly = Triangle TwoD TwoD TwoD

  à la place, le type @Point@ doit être utilisé:

> data Poly = Triangle Point Point Point

  \sshd{Type and noms de Constructeurs}\label{type-punning}

  Le Type et les noms de constructeurs peuvent être les mêmes, car ils ne peuvent en aucun cas
  être utilisés d'une manière ambigüe. Par exemple:

> data User = User String | Admin String

  qui déclare un type nommé @User@ avec deux constructeurs, @User@ et @Admin@.
  Utiliser ce type dans une fonction met en évidence la différence:

> whatUser (User _) = "utilisateur normal."
> whatUser (Admin _) = "administrateur."

  La littérature se réfère à cette pratique comme étant le \emph{transtypage de type}.

  \sshd{Variables de type}\label{type-variables}

  Déclarer des types de données \emph{polymorphiques} est aussi simple que d'ajouter des
  variables de type dans la déclaration:

> data Slot1 a = Slot1 a | Empty1

  Ceci déclare un type @Slot1@ avec deux constructeurs, @Slot1@ et @Empty1@. Le constructeur
  de @Slot1@ peut prendre un argument de type \emph{quelconque}, ce qui se représente par la
  variable de type @a@ ci-dessus.

  On peut également mélanger des variables de type et des types spécifiques dans les constructeurs:

> data Slot2 a = Slot2 a Int | Empty2

  Ci-dessus, le constructeur de @Slot2@ prend une valeur d'un type quelconque et une valeur de type @Int@.

  \sshd{Syntaxe des enregistrements}\label{record-syntax}

  Les arguments des Constructeurs peuvent être déclarés soit par position comme ci-dessus, ou
  en utilisant la syntaxe des enregistrements, qui permet de donner un nom à chacun des arguments. Par exemple,
  ici on déclare un type @Contact@ type avec les arguments appropriés:

> data Contact = Contact { ctName :: String
>       , ctEmail :: String
>       , ctPhone :: String }

  Ces noms sont nommés \emph{sélecteur} ou fonctions \emph{accesseur}
  et ne sont que cela, des fonctions. Elles doivent débuter par une lettre minuscule ou un
  caractère souligné et ne peuvent pas avoir le même nom qu'une fonction dans la portée en cours.
  D'ou le préfixe ``@ct@'' ci-dessus. Plusieurs constructeurs (du même type) peuvent utiliser
  la même fonction "accesseur" pour des valeurs de même type, mais cela peut être dangereux si
  l'accesseur n'est pas utilisé par tous les constructeurs. Considérez cet exemple plutôt
  réduit:

> data Con = Con { conValue :: String }
>   | Uncon { conValue :: String }
>   | Noncon
>
> whichCon con = "La valeur est " ++
>   conValue con

  Si @whichCon@ est appelé avec une valeur @Noncon@, une erreur d'exécution se produira.

  Finalement, comme expliqué ailleurs, ces noms peuvent être mis en oeuvre dans des motifs de
  correspondance, argument capturé et ``modification.''

  \sshd{Deriving}\label{deriving}

  De nombreux types ont des opérations en commun, pénibles à définir mais néanmoins nécessaires,
  telles que la capacité à convertir à partir et vers des chaînes de caractères, vérifier l'égalité, ou
  l'ordre dans une séquence. Ces capacités sont définies en tant que @classe de type@ en Haskell.

  Parce que sept de ces opérations sont tellement communes, Haskell fournit le mot réservé
  @deriving@ qui implémente automatiquement la @classe de type@ pour le type associé. Les sept @classe de type@
  supportés sont : @Eq@, @Read@, @Show@,
  @Ord@, @Enum@, @Ix@, et @Bounded@.

  Il existe deux variantes de @deriving@: La première est utilisée quand un type dérive seulement une classe:

> data Priority = Low | Medium | High
>   deriving Show

  La seconde est utilisée lorsque plusieurs classes sont dérivées:

> data Alarm = Soft | Loud | Deafening
>   deriving (Read, Show)

  Spécifier @deriving@ pour toute autre classe en dehors des sept ci-dessus constitue une erreur de syntaxe.

  \sshd{Contraintes de Classe}\label{class-constraints}

  Les types de données peuvent être déclarés avec des contraintes de classe sur les variables de type, mais
  cette pratique est découragée. Il est préférable de masquer les constructeurs de données ``bruts'' en
  utilisant le module system et à la place exporter des constructeurs ``intelligents'' qui appliquent les
  contraintes appropriées. Dans tous les cas, la syntaxe utilisée est:

> data (Num a) => SomeNumber a = Two a a
>   | Three a a a

  Ceci déclare un type @SomeNumber@ qui a un argument 'variable de type'. Les types valides sont ceux de la classe de type @Num@.

\shd{Deriving}

  Cf. la partie sur \hyperref[deriving]{@deriving@} sous le mot réservé @data@ keyword, page~\pageref{deriving}.

\shd{Do}\label{do}

  Le mot réservé @do@ indique que le code qui suit sera en contexte \emph{monadique}. Les instructions sont
  séparées par des sauts de lignes, l'affectation est marquée par @<-@, et une variante de @let@ sans le @in@
  est permise.

  \sshd{If et IO}\label{if-io}

  @if@ peut être subtil lorsque utilisé conjointement avec IO. Conceptuellement, ce n'est pas différent d'un
  @if@ dans n'importe quel autre contexte, mais intuitivement, c'est dur à construire. Considérez la fonction
  @doesFileExist@ de @System.Directory@:

< doesFileExist :: FilePath -> IO Bool

  L'instruction @if@ a cette ``signature'':

< if-then-else :: Bool -> a -> a -> a

  Donc, elle prend une valeur @booléenne@ et s'évalue à une autre valeur sur la base d'une condition. Vu la signature
  du type, il est évident que @doesFileExist@ ne peut pas être utilisé directement par @if@:

< wrong fileName =
<   if doesFileExist fileName
<     then ...
<     else ...

  Donc, @doesFileExist@ retourne une valeur @IO Bool@, alors que le @if@ requiert une valeur
  @booléenne@. A la place, la valeur correct doit être ``extraite'' en exécutant l'action IO:

> right1 fileName = do
>   exists <- doesFileExist fileName
>   if exists
>     then return 1
>     else return 0

  Notice the use of @return@. Because @do@ puts us ``inside'' the @IO@ monad, we
  can't ``get out'' except through @return@. Note that we don't have to use @if@
  inline here---we can also use @let@ to evaluate the condition and get a value
  first:

> right2 fileName = do
>   exists <- doesFileExist fileName
>   let result =
>         if exists
>           then 1
>           else 0
>   return result

  Again, notice where @return@ is. We don't put it in the @let@ statement.
  Instead we use it once at the end of the function.

  \sshd{Multiple @do@'s}\label{multiple-dos}

  When using @do@ with @if@ or @case@, another @do@ is required if either branch
  has multiple statements. An example with @if@:

> countBytes1 f =
>   do
>     putStrLn "Enter a filename."
>     args <- getLine
>     if length args == 0
>       -- no 'do'.
>       then putStrLn "No filename given."
>       else
>         -- multiple statements require
>         -- a new 'do'.
>         do
>           f <- readFile args
>           putStrLn ("The file is " ++
>             show (length f)
>             ++ " bytes long.")

  And one with @case@:

> countBytes2 =
>   do
>     putStrLn "Enter a filename."
>     args <- getLine
>     case args of
>       [] -> putStrLn "No args given."
>       file -> do
>        f <- readFile file
>        putStrLn ("The file is " ++
>          show (length f)
>          ++ " bytes long.")

  An alternative syntax uses semi-colons and braces. A @do@ is still required,
  but indention is unnecessary. This code shows a @case@ example, but the
  principle applies to @if@ as well:

> countBytes3 =
>   do
>     putStrLn "Enter a filename."
>     args <- getLine
>     case args of
>       [] -> putStrLn "No args given."
>       file -> do { f <- readFile file;
>        putStrLn ("The file is " ++
>          show (length f)
>          ++ " bytes long."); }

\shd{Export}

  See the section on \hyperref[module]{@module@} on page~\pageref{module}.

\shd{If, Then, Else}\label{if}

  Remember, @if@ always ``returns'' a value. It is an expression, not just a
  control flow statement. This function tests if the string given starts with a
  lower case letter and, if so, converts it to upper case:

> -- Use pattern-matching to
> -- get first character
> sentenceCase (s:rest) =
>  if isLower s
>    then toUpper s : rest
>    else s : rest
> -- Anything else is empty string
> sentenceCase _ = []

\shd{Import}

  See the section on \hyperref[module]{@module@} on page~\pageref{module}.

\shd{In}

  See \hyperref[let]{@let@} on page~\pageref{let}.

\shd{Infix, infixl and infixr}

  See the section on \hyperref[operators]{operators} on
  page~\pageref{operators}.

\shd{Instance}

  See the section on \hyperref[class]{@class@} on page~\pageref{class}.

\shd{Let}\label{let}

  Local functions can be defined within a function using @let@. The @let@
  keyword must always be followed by @in@. The @in@ must appear in the same
  column as the @let@ keyword.  Functions defined have access to all other
  functions and variables within the same scope (including those defined by
  @let@). In this example, @mult@ multiplies its argument @n@ by @x@, which was
  passed to the original @multiples@. @mult@ is used by map to give the
  multiples of x up to 10:

> multiples x =
>   let mult n = n * x
>   in map mult [1..10]

  @let@ ``functions'' with no arguments are actually constants and,
  once evaluated, will not evaluate again for that invocation of the
  outer function. This is useful for
  capturing common portions of your function and re-using them. Here
  is a silly example which gives the sum of a list of numbers and
  their average.  The @numbers@ definition captures the list of
  numbers from @1@ to @m@, and will only be evaulated once per
  invocation of @listStats@; similarly, @total@ and @avg@ are only
  evaluated once per invocation:

> listStats m =
>   let numbers = [1 .. m]
>       total = sum numbers
>       avg = total / m
>   in "total: " ++ show total ++
>      ", avg: " ++ show avg

  \sshd{Deconstruction}\label{deconstruction}

  The left-hand side of a @let@ definition can also destructure its argument, in
  case sub-components are to be accessed. This definition would extract the
  first three characters from a string

> firstThree str =
>   let (a:b:c:_) = str
>   in "Initial three characters are: " ++
>       show a ++ ", " ++
>       show b ++ ", and " ++
>       show c

  Note that this is different than the following, which only works if the string
  has exactly three characters:

> onlyThree str =
>   let (a:b:c:[]) = str
>   in "The characters given are: " ++
>       show a ++ ", " ++
>       show b ++ ", and " ++
>       show c

\shd{Of}

  See the section on \hyperref[case]{@case@} on page~\pageref{case}.

\shd{Module}\label{module}

  A module is a compilation unit which exports functions, types, classes,
  instances, and other modules. A module can only be defined in one file, though
  its exports may come from multiple sources. To make a Haskell file a module,
  just add a module declaration at the top:

< module MyModule where

  Module names must start with a capital letter but otherwise can include
  periods, numbers and underscores. Periods are used to give sense of structure,
  and Haskell compilers will use them as indications of the directory a
  particular source file is, but otherwise they have no meaning.

  The Haskell community has standardized a set of top-level module names such as
  @Data@, @System@, @Network@, etc. Be sure to consult them for an appropriate
  place for your own module if you plan on releasing it to the public.

  \sshd{Imports}\label{imports}

  The Haskell standard libraries are divided into a number of modules. The
  functionality provided by those libraries is accessed by importing into your
  source file. To import everything exported by a library, just use the
  module name:

< import Text.Read

  Everything means \emph{everything}: functions, data types and constructors,
  class declarations, and even other modules imported and then exported by the
  that module. Importing selectively is accomplished by giving a list of names
  to import. For example, here we import some functions from @Text.Read@:

< import Text.Read (readParen, lex)

  Data types can be imported in a number of ways. We can just import the type and
  no constructors:

< import Text.Read (Lexeme)

  Of course, this prevents our module from pattern-matching on the values of
  type @Lexeme@. We can import one or more constructors explicitly:

< import Text.Read (Lexeme(Ident, Symbol))

  All constructors for a given type can also be imported:

< import Text.Read (Lexeme(..))

  We can also import types and classes defined in the module:

< import Text.Read (Read, ReadS)

  In the case of classes, we can import the functions defined for a class using
  syntax similar to importing constructors for data types:

< import Text.Read (Read(readsPrec
<                       , readList))

  Note that, unlike data types, all class functions are imported unless
  explicitly excluded. To \emph{only} import the class, we use this syntax:

< import Text.Read (Read())

  \sshd{Exclusions}\label{exclusions}

  If most, but not all, names are to be imported from a module, it would be
  tedious to list them all. For that reason, imports can also be specified via
  the @hiding@ keyword:

< import Data.Char hiding (isControl
<                         , isMark)

  Except for instance declarations, any type, function, constructor or class can
  be hidden.

  \sshd{Instance Declarations}\label{instance-declarations}

  It must be noted that @instance@ declarations \emph{cannot} be excluded from
  import: all @instance@ declarations in a module will be imported when the
  module is imported.

  \sshd{Qualified Imports}\label{qualified-imports}

  The names exported by a module (i.e., functions, types, operators, etc.) can
  have a prefix attached through qualified imports. This is particularly useful
  for modules which have a large number of functions having the same name as
  @Prelude@ functions. @Data.Set@ is a good example:

< import qualified Data.Set as Set

  This form requires any function, type, constructor or other name exported by
  @Data.Set@ to now be prefixed with the \emph{alias} (i.e., @Set@) given. Here
  is one way to remove all duplicates from a list:

> removeDups a =
>   Set.toList (Set.fromList a)

  A second form does not create an alias. Instead, the prefix becomes the module
  name. We can write a simple function to check if a string is all upper case:

< import qualified Char

> allUpper str =
>   all Char.isUpper str

  Except for the prefix specified, qualified imports support the same syntax as
  normal imports. The name imported can be limited in the same ways as described
  above.

  \sshd{Exports}\label{exports}

  If an export list is not provided, then all functions, types, constructors,
  etc. will be available to anyone importing the module. Note that any imported
  modules are \emph{not} exported in this case. Limiting the names exported is
  accomplished by adding a parenthesized list of names before the @where@
  keyword:

< module MyModule (MyType
<   , MyClass
<   , myFunc1
<   ...)
< where

  The same syntax as used for importing can be used here to specify which
  functions, types, constructors, and classes are exported, with a few
  differences. If a module imports another module, it can also export that
  module:

< module MyBigModule (module Data.Set
<   , module Data.Char)
< where
<
< import Data.Set
< import Data.Char

  A module can even re-export itself, which can be useful when all local
  definitions and a given imported module are to be exported. Below we export
  ourselves and @Data.Set@, but not @Data.Char@:

< module AnotherBigModule (module Data.Set
<   , module AnotherBigModule)
< where
<
< import Data.Set
< import Data.Char

\shd{Newtype}\label{newtype}

  While @data@ introduces new values and @type@ just creates synonyms, @newtype@
  falls somewhere between. The syntax for @newtype@ is quite restricted---only
  one constructor can be defined, and that constructor can only take one
  argument. Continuing the above example, we can define a @Phone@ type as
  follows:

> newtype Home = H String
> newtype Work = W String
> data Phone = Phone Home Work

  As opposed to @type@, the @H@ and @W@ ``values'' on @Phone@ are \emph{not}
  just @String@ values. The typechecker treats them as entirely new types. That
  means our @lowerName@ function from above would not compile. The following
  produces a type error:

< lPhone (Phone hm wk) =
<   Phone (lower hm) (lower wk)

  Instead, we must use pattern-matching to get to the ``values'' to which we
  apply @lower@:

> lPhone (Phone (H hm) (W wk)) =
>   Phone (H (lower hm)) (W (lower wk))

  The key observation is that this keyword does not introduce a new value;
  instead it introduces a new type. This gives us two very useful properties:

  \begin{compactitem}
  \item No runtime cost is associated with the new type, since it does not
  actually produce new values. In other words, newtypes are absolutely free!

  \item The type-checker is able to enforce that common types such as @Int@ or
  @String@ are used in restricted ways, specified by the programmer.
  \end{compactitem}

  Finally, it should be noted that any @deriving@ clause which can be attached
  to a @data@ declaration can also be used when declaring a @newtype@.

\shd{Return}

  See \hyperref[do]{@do@} on page~\pageref{do}.

\shd{Type}\label{type}

  This keyword defines a \emph{type synonym} (i.e., alias). This keyword does
  not define a new type, like @data@ or @newtype@. It is useful for documenting
  code but otherwise has no effect on the actual type of a given function or
  value. For example, a @Person@ data type could be defined as:

<  data Person = Person String String

  where the first constructor argument represents their first name and the
  second their last. However, the order and meaning of the two arguments is not
  very clear. A @type@ declaration can help:

> type FirstName = String
> type LastName = String
> data Person = Person FirstName LastName

  Because @type@ introduces a synonym, type checking is not affected in any way.
  The function @lower@, defined as:

> lower s = map toLower s

  which has the type

< lower :: String -> String

  can be used on values with the type @FirstName@ or @LastName@ just as easily:

> lName (Person f l) =
>   Person (lower f) (lower l)

\shd{Where}\label{where}

  Similar to @let@, @where@ defines local functions and constants. The scope of
  a @where@ definition is the current function. If a function is broken into
  multiple definitions through pattern-matching, then the scope of a particular
  @where@ clause only applies to that definition. For example, the function
  @result@ below has a different meaning depending on the arguments given to the
  function @strlen@:

> strlen [] = result
>   where result = "No string given!"
> strlen f = result ++ " characters long!"
>   where result = show (length f)

  \sshd{Where vs. Let}\label{where-vs-let}

  A @where@ clause can only be defined at the level of a function definition.
  Usually, that is identical to the scope of @let@ definition. The only
  difference is when guards are being used. The scope of the @where@ clause
  extends over all guards. In contrast, the scope of a @let@ expression is only
  the current function clause \emph{and} guard, if any.

\hd{Contributors}\label{contributors}

  Mes remerciements à ceux qui ont contribué des modifications et des suggestions utiles:
Dave Bayer, Evgenij Belikov, Paul Butler, Jonas Devlieghere, Elisa Firth, Marc Fontaine, Brian
Gianforcaro, Cale Gibbard, Andrew Harris, Stephen Hicks, Kurt
Hutchinson, Johan Kiviniemi, Patrik Jansson, Adrian Neumann, Barak
Pearlmutter, Lanny Ripple, Markus Roberts, Holger Siegel, Falko
Spiller, Emiel van de Laar, Adam Vogt, Leif Warner, and Jeff Zaroyko.

\hd{Version}\label{version}

  Ceci est la version 2.9. Les sources peuvent être trouvés sur GitHub
  (\url{http://github.com/m4dc4p/cheatsheet}). La dernière version publiée
  du PDF peut être téléchargé depuis
  \url{http://cheatsheet.codeslower.com}.  Visitez CodeSlower.com
  (\url{http://blog.codeslower.com/}) pour d'autres projets et articles.

\end{multicols}
\end{document}

% vim:set tw=80:
