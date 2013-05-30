subroutine irmmf2(fid, nomamd, typent, nbrent, nbgrou,&
                  nomgen, nbec, nomast, prefix, typgeo,&
                  nomtyp, nmatyp, nufaen, nufacr, nogrfa,&
                  nofaex, tabaux, infmed, nivinf, ifm)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     ECRITURE DU MAILLAGE - FORMAT MED - LES FAMILLES - 2
!        -  -     -                 -         -          -
!-----------------------------------------------------------------------
!     L'ENSEMBLE DES FAMILLES EST L'INTERSECTION DE L'ENSEMBLE
!     DES GROUPES : UN NOEUD/MAILLE APPARAIT AU PLUS DANS 1 FAMILLE
!     TABLE  NUMEROS DES FAMILLES POUR LES NOEUDS  <-> TABLE  DES COO
!     TABLES NUMEROS DES FAMILLES POUR MAILLE/TYPE <-> TABLES DES CNX
!     PAR CONVENTION, LES FAMILLES DE NOEUDS SONT NUMEROTEES >0 ET LES
!     FAMILLES DE MAILLES SONT NUMEROTEES <0. LA FAMILLE NULLE EST
!     DESTINEE AUX NOEUDS / ELEMENTS SANS FAMILLE.
! ENTREES :
!   FID    : IDENTIFIANT DU FICHIER MED
!   NOMAMD : NOM DU MAILLAGE MED
!   TYPENT : TYPE D'ENTITES : 0, POUR DES NOEUDS, 1 POUR DES MAILLES
!   NBRENT : NOMBRE D'ENTITES A TRAITER
!   NBGROU : NOMBRE DE GROUPES D'ENTITES
!   NOMGEN : VECTEUR NOMS DES GROUPES D'ENTITES
!   NBEC   : NOMBRE D'ENTIERS CODES
!   NOMAST : NOM DU MAILLAGE ASTER
!   PREFIX : PREFIXE POUR LES TABLEAUX DES RENUMEROTATIONS
!   TYPGEO : TYPE MED POUR CHAQUE MAILLE
!   NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!   NMATYP : NOMBRE DE MAILLES PAR TYPE
! TABLEAUX DE TRAVAIL
!   NUFAEN : NUMERO DE FAMILLE POUR CHAQUE ENTITE
!            PAR DEFAUT, L'ALLOCATION AVEC JEVEUX A TOUT MIS A 0. CELA
!            SIGNIFIE QUE LES ENTITES APPARTIENNENT A LA FAMILLE NULLE.
!   NUFACR : NUMERO DE FAMILLES CREES. AU MAXIMUM, AUTANT QUE D'ENTITES
!   NOGRFA : NOM DES GROUPES ASSOCIES A CHAQUE FAMILLE.
!   NOFAEX = NOMS DES FAMILLES DEJA CREEES
!   TABAUX : PRESENCE D UNE ENTITE DANS UN GROUPE
! DIVERS
!   INFMED : NIVEAU DES INFORMATIONS SPECIFIQUES A MED A IMPRIMER
!   NIVINF : NIVEAU DES INFORMATIONS GENERALES
!   IFM    : UNITE LOGIQUE DU FICHIER DE MESSAGE
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
!
    include 'asterfort/desgfa.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mdnofa.h'
    include 'asterfort/mffamc.h'
    include 'asterfort/mffame.h'
    include 'asterfort/nomgfa.h'
    include 'asterfort/setgfa.h'
    include 'asterfort/u2mesg.h'
    integer :: fid
    integer :: typgeo(*), nmatyp(*)
    integer :: typent, nbrent, nbgrou
    integer :: nbec
    integer :: nufaen(nbrent), nufacr(nbrent), tabaux(*)
    integer :: infmed
    integer :: ifm, nivinf
!
    character(len=6) :: prefix
    character(len=8) :: nomast
    character(len=24) :: nomgen(*)
    character(len=8) :: nomtyp(*)
    character(len=*) :: nofaex(*)
    character(len=80) :: nogrfa(nbgrou)
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMMF2' )
!
    integer :: ntymax
    parameter (ntymax = 69)
    integer :: edmail, ednoeu
    parameter (edmail=0, ednoeu=3)
    integer :: tygeno
    parameter (tygeno=0)
!
    integer :: codret
    integer :: iaux, jaux, kaux
    integer :: numfam, nfam
    integer :: ityp
    integer :: nbeg, ige, ient, entfam, nbgnof, natt
    integer :: jgren
    integer :: tbaux(1)
!
    character(len=7) :: saux07
    character(len=8) :: saux08
    character(len=9) :: saux09
    character(len=64) :: nomfam
!      REAL*8 TPS1(4),TPS2(4),TPS3(4)
!
!====
! 1. PREALABLES
!====
!
    if (nivinf .ge. 2) then
!
        write (ifm,1001) nompro
        1001 format( 60('-'),/,'DEBUT DU PROGRAMME ',a)
!
    endif
!
!     NATT = NOMBRE D'ATTRIBUTS DANS UNE FAMILLE : JAMAIS. ELLES NE SONT
!            DEFINIES QUE PAR LES GROUPES
    natt = 0
!
!     NFAM = NUMERO DE LA DERNIERE FAMILLE ENREGISTREE (DE 0 A N>0)
!     FAMILLE 0 = ENTITES N'APPARTENANT A AUCUN GROUPE
    nfam = 0
!
!====
! 2. EN PRESENCE DE GROUPES, ON CREE DES FAMILLES
!====
!
    if (nbgrou .ne. 0) then
!
        if (typent .eq. tygeno) then
            saux09 = '.GROUPENO'
        else
            saux09 = '.GROUPEMA'
        endif
!
! 2.1. ==> BUT DE L'ETAPE 2.1 : CONNAITRE POUR CHAQUE ENTITE SES GROUPES
!          D'APPARTENANCE
!
        do 21 , ige = 1 , nbgrou
!
        call jeveuo(jexnum(nomast//saux09, ige), 'L', jgren)
        call jelira(jexnum(nomast//saux09, ige), 'LONMAX', nbeg, saux08)
!
        if (infmed .ge. 2) then
            if (typent .eq. tygeno) then
                saux07 = 'NOEUDS '
            else
                saux07 = 'MAILLES'
            endif
            write (ifm,2001) nomgen(ige), nbeg, saux07
        endif
        2001 format( '. GROUPE ',a,' :',i12,1x,a)
!
!         POUR CHAQUE GROUPE, ON BOUCLE SUR LES ENTITES QU'IL CONTIENT.
!
        do 211 , iaux = 1 , nbeg
!
!           DEBUT VECTEUR ENTIER CODE POUR ENTITE IENT DANS JENTXG
        ient = zi(jgren-1+iaux)
        if (ient .ne. 0) then
!             ENREGISTREMENT APPARTENANCE DU ENTITE AU GROUPE
            call setgfa(tabaux(1+(ient-1)*nbec), ige)
!             MISE A -1 DU NUM DE FAMILLE POUR CETTE ENTITE DANS NUFAEN
!             POUR INDIQUER QU'ELLE APPARTIENT AU MOINS A UN GROUPE
            nufaen(ient) = 1
        endif
!
211      continue
!
21      continue
!
! 2.2. ==> BUT DE L'ETAPE 2.2 : FAIRE LA PARTITION EN FAMILLE ET NOTER :
!          . LE NUMERO DE LA 1ER ENTITE DE LA FAMILLE
!          . LE NUMERO DE FAMILLE DE CHAQUE ENTITE
!
!          ON BOUCLE SUR LES ENTITES APPARTENANT AU MOINS A UN GROUPE
!          ET ON LES RASSEMBLE PAR IDENTITE D'APPARTENANCE.
!          LES FAMILLES SONT NUMEROTEES DANS L'ORDRE D'APPARITION
!          ATTENTION : CET ALGORITHME A ETE OPTIMISE LE 6/9/2002
!                      ETRE PRUDENT DANS LES AMELIORATIONS FUTURES ...
!                      LES SITUATIONS PENALISANTES SONT CELLES-CI :
!                      QUELQUES DIZAINES DE MILLIERS D'ENTITES ET
!                      QUELQUES CENTAINES DE GROUPES
!                      EXEMPLE : ON EST PASSE D'UNE VINGTAINE D'HEURES
!                      A 3 MINUTES AVEC UN GROS MAILLAGE :
!                      . 426 817 NOEUDS EN 57 GROUPES ET
!                      . 418 514 MAILLES EN 8 629 GROUPES.
!
        do 22 , ient = 1 , nbrent
!
        if (nufaen(ient) .ne. 0) then
!
!         BOUCLE 221 : ON PARCOURT TOUTES LES FAMILLES DEJA VUES.
!         POUR CHACUNE D'ELLES, ON COMPARE LES GROUPES ASSOCIES ET LES
!         GROUPES DE L'ENTITE COURANTE :
!         MEME COMPOSITION DE GROUPES <==> MEMES ENTIERS CODES
!         . SI C'EST LA MEME COMPOSITION DE GROUPES, LA FAMILLE EST LA
!           MEME. ON DONNE DONC LE NUMERO DE FAMILLE L'ENTITE COURANTE.
!         . SI ON N'A TROUVE AUCUNE FAMILLE, C'EST QU'UNE NOUVELLE
!           FAMILLE VIENT D'APPARAITRE. ON STOCKE SES CARACTERISTIQUES.
!
            jaux = nbec*(ient-1)
!
            do 221 , numfam = 1 , nfam
!
            entfam = nufacr(numfam)
!
            kaux = nbec*(entfam-1)
!
            do 222 , iaux = 1 , nbec
            if (tabaux(jaux+iaux) .ne. tabaux(kaux+iaux)) then
                goto 221
            endif
222          continue
!
!             ON A TROUVE UNE FAMILLE AVEC LA MEME COMPOSITION :
!             . ON NOTE QUE LA FAMILLE EST LA MEME
!             . ON PASSE A L'ENTITE SUIVANTE
!
            nufaen(ient) = nufaen(entfam)
            goto 22
!
221          continue
!
!           AUCUN ENTITE NE CORRESPONDAIT : ON CREE UNE NOUVELLE FAMILLE
            nfam = nfam + 1
!           ON MEMORISE CE NUMERO DE FAMILLE POUR L'ENTITE COURANTE
!           ATTENTION : LA CONVENTION MED VEUT QUE LE NUMERO SOIT
!           POSITIF POUR LES FAMILLES DE NOEUDS, NEGATIF POUR
!           LES MAILLES
            nufaen(ient) = nfam
            if (typent .ne. tygeno) then
                nufaen(ient) = -nufaen(ient)
            endif
!           ON INDIQUE OU SE TROUVE LA 1ERE REFERENCE A CETTE FAMILLE
!           DANS LE VECTEUR NUFACR POUR EVITER DE PERDRE SON TEMPS APRES
            nufacr(nfam) = ient
!
        endif
!
22      continue
!
! 2.3. ==> BUT DE L'ETAPE 2.3 : CREATION DES FAMILLES D'ENTITES ET LES
!          ECRIRE DANS LE FICHIER
!
!          ON PARCOURT LES FAMILLES REPERTORIEES.
!          ON MEMORISE LES NOMS ET NUMEROS DES GROUPES QUI LA
!          CARACTERISENT. POUR CELA, ON SE BASE SUR LE PREMIER ENTITE
!          QUI EN FAIT PARTIE.
!
        do 23 , iaux = 1 , nfam
!
! 2.3.1. ==> DETERMINATION DE LA FAMILLE : NOM, NOMS ET NUMEROS DES
!              GROUPES ASSOCIES
!
        numfam = iaux
        if (typent .ne. tygeno) then
            numfam = -numfam
        endif
!
!         NUMERO DE LA 1ERE ENTITE FAISANT REFERENCE A CETTE FAMILLE
        ient = nufacr(iaux)
!
!         NB ET NOMS+NUMS DES GROUPES ASSOCIES A LA FAMILLE
        call nomgfa(nomgen, nbgrou, tabaux(1+(ient-1)*nbec), nogrfa, nbgnof)
!
!         NOM DE LA FAMILLE : ON LE CONSTRUIT A PARTIR DES NOMS
!         DE GROUPES
!
        jaux = iaux - 1
        call mdnofa(numfam, nogrfa, nbgnof, jaux, nofaex,&
                    nomfam)
!
! 2.3.2. ==> INFORMATION EVENTUELLE
!
        if (infmed .ge. 2) then
            jaux = 0
            do 232 , ient = 1 , nbrent
            if (nufaen(ient) .eq. numfam) then
                jaux = jaux + 1
            endif
232          continue
            if (typent .eq. tygeno) then
                kaux = 0
            else
                kaux = jaux
                jaux = 0
            endif
            call desgfa(typent+1, numfam, nomfam, nbgnof, nogrfa,&
                        natt, tbaux, jaux, kaux, ifm,&
                        codret)
        endif
!
! 2.3.3. ==> ECRITURE DES CARACTERISTIQUES DE LA FAMILLE
!
        call mffamc(fid, nomamd, nomfam, numfam, nbgnof,&
                    nogrfa, codret)
        if (codret .ne. 0) then
            saux08='MFFAMC  '
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
23      continue
!
    endif
!
!====
! 3. ECRITURE DE LA TABLE DES NUMEROS DE FAMILLES DES ENTITES
!    CELA SE FAIT PAR TYPE. ON REUTILISE LES VECTEURS CONTENANT
!    LES NUMEROS D'ENTITES/TYPE
!====
!
! 3.1. ==> ECRITURE DANS LE CAS DES NOEUDS
!
    if (typent .eq. tygeno) then
!
        call mffame(fid, nomamd, nufaen, nbrent, ednoeu,&
                    tygeno, codret)
!
        if (codret .ne. 0) then
            saux08='MFFAME  '
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
! 3.2. ==> ECRITURE DANS LE CAS DES MAILLES : IL FAUT PASSER PAR LA
!          RENUMEROTATION ASTER-MED
!
    else
!
        do 32 , ityp = 1 , ntymax
!
        if (nmatyp(ityp) .ne. 0) then
!
!           RECUPERATION DU TABLEAU DES RENUMEROTATIONS
!
            call jeveuo('&&'//prefix//'.NUM.'//nomtyp(ityp), 'L', kaux)
!
!           CREATION VECTEUR NUMEROS DE FAMILLE POUR LES MAILLES / TYPE
!
            do 321 , iaux = 1 , nmatyp(ityp)
            tabaux(iaux) = nufaen(zi(kaux-1+iaux))
321          continue
!
            call mffame(fid, nomamd, tabaux, nmatyp(ityp), edmail,&
                        typgeo(ityp), codret)
!
            if (codret .ne. 0) then
                saux08='MFFAME  '
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
!
        endif
!
32      continue
!
    endif
!
    if (nivinf .ge. 2) then
!
        write (ifm,4001) nompro
        4001 format(/,'FIN DU PROGRAMME ',a,/,60('-'))
!
    endif
!
end subroutine
