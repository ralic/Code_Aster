subroutine enerca(valinc, dep0, vit0, depl1, vite1,&
                  masse, amort, rigid, fexte, famor,&
                  fliai, fnoda, fcine, lamort, ldyna,&
                  lexpl, sdener, schema)
    implicit none
!
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
! person_in_charge: ludovic.idoux at edf.fr
! ----------------------------------------------------------------------
!     CALCUL DES ENERGIES
!     DONNEES (IN) :
!     VALINC : NECESSAIRE A LA RECUPERATION DU NOM DES GRANDEURS
!              NODALES DANS MECA_NON_LINE.
!     FEXTE  : TABLEAU DE TAILLE 2*NEQ.
!              LES FORCES EXTERIEURES A L INSTANT N SONT STOCKEES
!              DE 1 A NEQ.
!              LES FORCES EXTERIEURES A L INSTANT N+1 SONT STOCKEES
!              DE NEQ+1 A 2*NEQ.
!     FAMOR  : MEME PRINCIPE. CONTIENT LES FORCES D AMORTISSEMENT
!              MODAL UNIQUEMENT.
!     FLIAI  : MEME PRINCIPE. CONTIENT LES FORCES D IMPEDANCE POUR
!              LES FRONTIERES ABSORBANTES (A ENRICHIR PAR LA SUITE).
!     FNODA  : MEME PRINCIPE. CONTIENT LES FORCES INTERNES.
!     FCINE  : DE DIMENSION NEQ. CONTIENT LES INCREMENTS DE
!              DEPLACEMENT IMPOSE A L INSTANT N.
!     LAMORT : INDIQUE SI UNE MATRICE D AMORTISSEMENT EXISTE.
!              UNIQUEMENT SI AMORTISSEMENT DE RAYLEIGH.
!     LDYNA  : INDIQUE SI LE CALCUL EST DYNAMIQUE. DECLENCHE LA
!              LECTURE DE LA MATRICE MASSE ET DE LA VITESSE,
!              ET LE CALCUL DE ECIN.
!     LEXPL  : INDIQUE SI LE CALCUL EST UN DYNA_NON_LINE EN
!              EXPLICITE (DIFF_CENTRE OU TCHAMWA).
!              LES LAGRANGES SONT ALORS PORTES PAR LA MATRICE MASSE.
! ----------------------------------------------------------------------
!  IN  : VALINC    : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!  IN  : DEP0      : TABLEAU DES DEPLACEMENTS A L INSTANT N
!  IN  : VIT0      : TABLEAU DES VITESSES A L INSTANT N
!  IN  : DEPL1     : TABLEAU DES DEPLACEMENTS A L INSTANT N+1
!  IN  : VITE1     : TABLEAU DES VITESSES A L INSTANT N+1
!  IN  : MASSE     : MATRICE DE MASSE
!  IN  : AMORT     : MATRICE D AMORTISSEMENT
!  IN  : RIGID     : MATRICE DE RIGIDITE
!  IN  : FEXTE     : VECTEUR DES FORCES EXTERIEURES
!  IN  : FAMOR     : VECTEUR DES FORCES D AMORTISSEMENT MODAL
!  IN  : FLIAI     : VECTEUR DES FORCES DE LIAISON
!  IN  : FNODA     : VECTEUR DES FORCES NODALES
!  IN  : FCINE     : VECTEUR DES INCREMENTS DE DEPLACEMENT IMPOSE
!  IN  : LAMORT    : LOGICAL .TRUE. SI LA MATRICE AMORTISSEMENT EXISTE
!  IN  : LDYNA     : LOGICAL .TRUE. SI CALCUL DYNAMIQUE
!  IN  : LEXPL     : LOGICAL .TRUE. SI CALCUL EXPLICITE DANS DNL
!  IN  : SDENER    : SD ENERGIE
!  IN  : SCHEMA    : NOM DU SCHEMA POUR DYNA_LINE_TRAN
!
! ----------------------------------------------------------------------
! DECLARATION PARAMETRES D'APPELS
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/ddlphy.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/nmchex.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
!
    character(len=19) :: valinc(*), masse, amort, rigid, sdener
    real(kind=8) :: dep0(*), vit0(*), depl1(*), vite1(*)
    real(kind=8) :: fexte(*), famor(*), fliai(*), fnoda(*), fcine(*)
    logical :: lamort, ldyna, lexpl
    character(len=8) :: schema
!
!
!
!
! ----------------------------------------------------------------------
! DECLARATION VARIABLES LOCALES
! ----------------------------------------------------------------------
    integer :: iaux, neq, nbcol, long
    integer :: jdeeq, imdv, icvmoz, iener
    integer :: imasse, iamort, irigid
    integer :: iumoy, iupmum, iumoyz, iupmuz
    integer :: ivmoy, ivpmvm, ivmoyz, ivpmvz
    integer :: ikumoy, ikumoz, imumoy, imumoz, ifmoy, idesc
    character(len=24) :: numedd
    character(len=19) :: depplu
    character(len=11) :: forma
    character(len=40) :: formb, formc
    real(kind=8) :: wint, wext, liai, ecin, amor, wsch
!
! ----------------------------------------------------------------------
! CORPS DU PROGRAMME
! ----------------------------------------------------------------------
!
    call jemarq()
!
    wint=0.d0
    wext=0.d0
    liai=0.d0
    ecin=0.d0
    amor=0.d0
    wsch=0.d0
!
!
    if (ldyna) then
! ----------------------------------------------------------------------
! CALCUL DYNAMIQUE (DYNA_NON_LINE OU DYNA_LINE_TRAN)
! - RECUPERATION DE LA MATRICE DE MASSE
! - RECUPERATION DE LA MATRICE D AMORTISSEMENT SI ELLE EXISTE
! - CREATION DES VECTEURS DE DEPLACEMENT MOYEN ET D INCREMENT
!   DE DEPLACEMENT
! - CREATION DES VECTEURS DE VITESSE MOYENNE ET D INCREMENT DE VITESSE
! - RECUPERATION DE LA MATRICE DE RIGIDITE SI ELLE PORTE LES LAGRANGE
! ----------------------------------------------------------------------
        call jeveuo(masse//'.&INT', 'L', imasse)
        if (lamort) then
            call jeveuo(amort//'.&INT', 'L', iamort)
        endif
        neq=zi(imasse+2)
        if (.not.lexpl) then
            call jeveuo(rigid//'.&INT', 'L', irigid)
        endif
        call wkvect('&&ENERCA.DESC', 'V V K8', neq, idesc)
        call wkvect('&&ENERCA.UMOY', 'V V R', neq, iumoy)
        call wkvect('&&ENERCA.UPMUM', 'V V R', neq, iupmum)
        call wkvect('&&ENERCA.UMOYZ', 'V V R', neq, iumoyz)
        call wkvect('&&ENERCA.UPMUMZ', 'V V R', neq, iupmuz)
        call wkvect('&&ENERCA.VMOY', 'V V R', neq, ivmoy)
        call wkvect('&&ENERCA.VPMVM', 'V V R', neq, ivpmvm)
        call wkvect('&&ENERCA.VMOYZ', 'V V R', neq, ivmoyz)
        call wkvect('&&ENERCA.VPMVMZ', 'V V R', neq, ivpmvz)
        do iaux = 1, neq
            zr(iumoy-1+iaux)=(depl1(iaux)+dep0(iaux))*5.d-1
            zr(iupmum-1+iaux)=depl1(iaux)-dep0(iaux)
            zr(ivmoy-1+iaux)=(vite1(iaux)+vit0(iaux))*5.d-1
            zr(ivpmvm-1+iaux)=vite1(iaux)-vit0(iaux)
        end do
        call dcopy(neq, zr(iumoy), 1, zr(iumoyz), 1)
        call dcopy(neq, zr(iupmum), 1, zr(iupmuz), 1)
        call dcopy(neq, zr(ivmoy), 1, zr(ivmoyz), 1)
        call dcopy(neq, zr(ivpmvm), 1, zr(ivpmvz), 1)
        call dismoi('NOM_NUME_DDL', masse, 'MATR_ASSE', repk=numedd)
        call jeveuo(numedd(1:14)//'.NUME.DEEQ', 'L', jdeeq)
        if (sdener(1:8) .eq. '&&OP0070') then
! ON NE GARDE QUE LES DDL NODAUX PHYSIQUES
            call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
        else if (sdener(1:8).eq.'&&OP0048') then
            depplu=schema//'.DEPL1     '
            if (schema .eq. '&&DLADAP') then
                depplu=schema//'.DEP2 '
            endif
        endif
        call ddlphy(depplu, neq, zr(iupmuz), zk8(idesc))
        call ddlphy(depplu, neq, zr(ivmoyz), zk8(idesc))
        call ddlphy(depplu, neq, zr(ivpmvz), zk8(idesc))
! ON ENLEVE UNIQUEMENT LES LAGRANGES DES CONDITIONS DE DIRICHLET
        call zerlag(neq, zi(jdeeq), vectr=zr(iumoyz))
    else
! --------------------------------------------------------------------
! CALCUL STATIQUE (STAT_NON_LINE)
! - CREATION DES VECTEURS DE DEPLACEMENT MOYEN ET D INCREMENT
!   DE DEPLACEMENT
! - RECUPERATION DE LA MATRICE DE RIGIDITE POUR OBTENIR LES LAGRANGES
! --------------------------------------------------------------------
        call jeveuo(rigid//'.&INT', 'L', irigid)
        neq=zi(irigid+2)
        call wkvect('&&ENERCA.DESC', 'V V K8', neq, idesc)
        call wkvect('&&ENERCA.UMOY', 'V V R', neq, iumoy)
        call wkvect('&&ENERCA.UPMUM', 'V V R', neq, iupmum)
        call wkvect('&&ENERCA.UMOYZ', 'V V R', neq, iumoyz)
        call wkvect('&&ENERCA.UPMUMZ', 'V V R', neq, iupmuz)
        do iaux = 1, neq
            zr(iumoy-1+iaux)=(depl1(iaux)+dep0(iaux))*5.d-1
            zr(iupmum-1+iaux)=depl1(iaux)-dep0(iaux)
        end do
        call dcopy(neq, zr(iumoy), 1, zr(iumoyz), 1)
        call dcopy(neq, zr(iupmum), 1, zr(iupmuz), 1)
        call dismoi('NOM_NUME_DDL', rigid, 'MATR_ASSE', repk=numedd)
        call jeveuo(numedd(1:14)//'.NUME.DEEQ', 'L', jdeeq)
        call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
! ON NE GARDE QUE LES DDL NODAUX PHYSIQUES
        call ddlphy(depplu, neq, zr(iupmuz), zk8(idesc))
! ON ENLEVE UNIQUEMENT LES LAGRANGES DES CONDITIONS DE DIRICHLET
        call zerlag(neq, zi(jdeeq), vectr=zr(iumoyz))
    endif
! --------------------------------------------------------------------
! WINT : TRAVAIL REEL DES EFFORTS CALCULE COMME LE TRAVAIL DES FORCES
!        INTERNES
! - SI DYNA_LINE_TRAN : EGAL A L ENERGIE DE DEFORMATION ELASTIQUE
! - SI MECA_NON_LINE : TRAVAIL DES FORCES INTERNES
! --------------------------------------------------------------------
    call wkvect('&&ENERCA.FMOY', 'V V R', neq, ifmoy)
    if (sdener(1:8) .eq. '&&OP0048') then
        call wkvect('&&ENERCA.KUMOYZ', 'V V R  ', neq, ikumoz)
        call mrmult('ZERO', irigid, zr(iumoyz), zr(ikumoz), 1,&
                    .true.)
        wint=ddot(neq,zr(iupmuz),1,zr(ikumoz),1)
    else
        do iaux = 1, neq
            zr(ifmoy-1+iaux)=(fnoda(iaux)+fnoda(iaux+neq))*5.d-1
        end do
        wint=ddot(neq,zr(iupmuz),1,zr(ifmoy),1)
    endif
! --------------------------------------------------------------------
! ECIN : ENERGIE CINETIQUE
! - UNIQUEMENT SI CALCUL DYNAMIQUE
! --------------------------------------------------------------------
    if (ldyna) then
        call wkvect('&&ENERCA.MDV', 'V V R  ', neq, imdv)
        call mrmult('ZERO', imasse, zr(ivpmvz), zr(imdv), 1,&
                    .true.)
        ecin=ddot(neq,zr(ivmoyz),1,zr(imdv),1)
    endif
! --------------------------------------------------------------------
! WEXT : TRAVAIL DES EFFORTS EXTERIEURS
! --------------------------------------------------------------------
    wext=0.d0
! 1. CONTRIBUTION AFFE_CHAR_CINE (MECA_NON_LINE UNIQUEMENT)
    if (sdener(1:8) .eq. '&&OP0070') then
        wext=ddot(neq,zr(ifmoy),1,fcine(1),1)
    endif
! 2. CONTRIBUTION DE Bt.LAMBDA (DIRICHLETS) POUR OP0048
    if (sdener(1:8) .eq. '&&OP0048') then
        if (lexpl) then
! LAGRANGES PORTES PAR LA MATRICE DE MASSE
            call wkvect('&&ENERCA.MUMOY', 'V V R', neq, imumoy)
            call wkvect('&&ENERCA.MUMOYZ', 'V V R', neq, imumoz)
            call mrmult('ZERO', imasse, zr(iumoy), zr(imumoy), 1,&
                        .true.)
            call mrmult('ZERO', imasse, zr(iumoyz), zr(imumoz), 1,&
                        .true.)
            do iaux = 1, neq
                zr(ifmoy-1+iaux)=zr(imumoz-1+iaux)-zr(imumoy-1+iaux)
            end do
            wext = wext + ddot(neq,zr(ifmoy),1,zr(iupmuz),1)
        else
! LAGRANGES PORTES PAR LA MATRICE DE RIGIDITE
            call wkvect('&&ENERCA.KUMOY', 'V V R  ', neq, ikumoy)
            call mrmult('ZERO', irigid, zr(iumoy), zr(ikumoy), 1,&
                        .true.)
            do iaux = 1, neq
                zr(ifmoy-1+iaux)=zr(ikumoz-1+iaux)-zr(ikumoy-1+iaux)
            end do
            wext = wext + ddot(neq,zr(ifmoy),1,zr(iupmuz),1)
        endif
    endif
! 3. CONTRIBUTION DES NEUMANN
    do iaux = 1, neq
        zr(ifmoy-1+iaux)=(fexte(iaux)+fexte(iaux+neq))*5.d-1
    end do
! GLUT : LA CONTRIBUTION DE LA FORCE QUI TRAVAILLE EN UN POINT OU
! LE DEPLACEMENT EST IMPOSE EST PRIS EN COMPTE DANS WEXT1 POUR
! LES AFFE_CHAR_CINE ET DANS WEXT2 POUR AFFE_CHAR_MECA. IL PEUT
! ARRIVER QU'ELLE SOIT REPRISE EN COMPTE DANS WEXT3 SI ON INTRODUIT
! UN CHARGEMENT VOLUMIQUE (PESANTEUR). IL FAUT DONC METTRE A ZERO
! CERTAINS TERMES DES EFFORTS EXTERIEURS.
    do iaux = 1, neq
        if (fcine(iaux) .ne. 0.d0) then
            zr(ifmoy-1+iaux)=0.d0
        endif
    end do
    wext = wext + ddot(neq,zr(ifmoy),1,zr(iupmuz),1)
! --------------------------------------------------------------------
! LIAI : ENERGIE DISSIPEE PAR LES LIAISONS
! - UNIQUEMENT IMPE_ABSO POUR DYNA_LINE_TRAN
! --------------------------------------------------------------------
    do iaux = 1, neq
        zr(ifmoy-1+iaux)=(fliai(iaux)+fliai(iaux+neq))*5.d-1
    end do
    liai=ddot(neq,zr(iupmuz),1,zr(ifmoy),1)
! --------------------------------------------------------------------
! AMOR : ENERGIE DISSIPEE PAR AMORTISSEMENT
! - UNIQUEMENT SI CALCUL DYNAMIQUE
! --------------------------------------------------------------------
    if (ldyna) then
        do iaux = 1, neq
            zr(ifmoy-1+iaux)=(famor(iaux)+famor(iaux+neq))*5.d-1
        end do
        amor=ddot(neq,zr(iupmuz),1,zr(ifmoy),1)
        if (lamort) then
            if (zi(iamort+3) .eq. 1) then
                call wkvect('&&ENERCA.CVMOYZ', 'V V R', neq, icvmoz)
                call mrmult('ZERO', iamort, zr(ivmoyz), zr(icvmoz), 1,&
                            .true.)
                amor = amor + ddot(neq,zr(iupmuz),1,zr(icvmoz),1)
            else
                call wkvect('&&ENERCA.CVMOYZ', 'V V C', neq, icvmoz)
                call mrmult('ZERO', iamort, zr(ivmoyz), zr(icvmoz), 1,&
                            .true.)
                amor = amor + ddot(neq,zr(iupmuz),1,zr(icvmoz),1)
            endif
        endif
    endif
! --------------------------------------------------------------------
! WSCH : ENERGIE DISSIPEE PAR LE SCHEMA
! --------------------------------------------------------------------
    wsch=wext-ecin-wint-amor-liai
! --------------------------------------------------------------------
! MISE A JOUR DES ENERGIES
! - ORDRE : WEXT - ECIN - WINT - AMOR - LIAI - WSCH
! --------------------------------------------------------------------
    call jeveuo(sdener//'.VALE', 'E', iener)
    nbcol=4
    zr(iener-1+1)=zr(iener-1+1)+wext
    zr(iener-1+3)=zr(iener-1+3)+wint
    zr(iener-1+6)=zr(iener-1+6)+wsch
    if (ldyna) then
        zr(iener-1+2)=zr(iener-1+2)+ecin
        zr(iener-1+4)=zr(iener-1+4)+amor
        nbcol=nbcol+2
    endif
    zr(iener-1+5)=zr(iener-1+5)+liai
    if ((zr(iener-1+5).ne.0.d0) .or. (liai.ne.0.d0)) then
        nbcol=nbcol+1
    endif
! --------------------------------------------------------------------
! AFFICHAGE DU BILAN
! MINIMUM : 4 COLONNES (TITRE, WEXT, WINT, WSCH)
! 5 COLONNES : AJOUT DE LIAI
! 6 COLONNES : AJOUT DE ECIN ET AMOR
! 7 COLONNES : AJOUT DE LIAI, ECIN ET AMOR
! --------------------------------------------------------------------
    long=18+14*(nbcol-1)+1
    write(forma,1001) long
    write(6,forma) ('-',iaux=1,long)
    write(formb,1002) nbcol-1
    write(formc,1003) nbcol-1
    if (nbcol .eq. 4) then
        write(6,formb) '|','BILAN D''ENERGIE','|','  TRAV_EXT   ','|',&
     &                 '  ENER_TOT   ','|','  DISS_SCH   ','|'
        write(6,formc) '|','  PAS COURANT  ','|',wext,'|',wint,&
     &                 '|',wsch,'|'
        write(6,formc) '|','     TOTAL     ','|',zr(iener-1+1),&
     &                 '|',zr(iener-1+3),'|',zr(iener-1+6),'|'
    else if (nbcol.eq.5) then
        write(6,formb) '|','BILAN D''ENERGIE','|','  TRAV_EXT   ','|',&
     &                 '  ENER_TOT   ','|','  TRAV_LIAI  ','|',&
     &                 '  DISS_SCH   ','|'
        write(6,formc) '|','  PAS COURANT  ','|',wext,'|',wint,'|',liai,&
     &                 '|',wsch,'|'
        write(6,formc) '|','     TOTAL     ','|',zr(iener-1+1),&
     &                 '|',zr(iener-1+3),'|',zr(iener-1+5),&
     &                 '|',zr(iener-1+6),'|'
    else if (nbcol.eq.6) then
        write(6,formb) '|','BILAN D''ENERGIE','|','  TRAV_EXT   ','|',&
     &                 '  ENER_TOT   ','|','  ENER_CIN   ','|',&
     &                 '  TRAV_AMOR  ','|','  DISS_SCH   ','|'
        write(6,formc) '|','  PAS COURANT  ','|',wext,'|',wint,'|',ecin,&
     &                 '|',amor,'|',wsch,'|'
        write(6,formc) '|','     TOTAL     ','|',zr(iener-1+1),&
     &                 '|',zr(iener-1+3),'|',zr(iener-1+2),&
     &                 '|',zr(iener-1+4),'|',zr(iener-1+6),'|'
    else if (nbcol.eq.7) then
        write(6,formb) '|','BILAN D''ENERGIE','|','  TRAV_EXT   ','|',&
     &                 '  ENER_TOT   ','|','  ENER_CIN   ','|',&
     &                 '  TRAV_AMOR  ','|','  TRAV_LIAI  ','|',&
     &                 '  DISS_SCH   ','|'
        write(6,formc) '|','  PAS COURANT  ','|',wext,'|',wint,'|',ecin,&
     &                 '|',amor,'|',liai,'|',wsch,'|'
        write(6,formc) '|','     TOTAL     ','|',zr(iener-1+1),&
     &                 '|',zr(iener-1+3),'|',zr(iener-1+2),&
     &                 '|',zr(iener-1+4),'|',zr(iener-1+5),&
     &                 '|',zr(iener-1+6),'|'
    endif
    write(6,forma) ('-',iaux=1,long)
!
! --------------------------------------------------------------------
! MENAGE
! --------------------------------------------------------------------
    call jedetr('&&ENERCA.CVMOYZ')
    call jedetr('&&ENERCA.DESC')
    call jedetr('&&ENERCA.FMOY')
    call jedetr('&&ENERCA.KUMOY')
    call jedetr('&&ENERCA.KUMOYZ')
    call jedetr('&&ENERCA.MDV')
    call jedetr('&&ENERCA.MUMOY')
    call jedetr('&&ENERCA.MUMOYZ')
    call jedetr('&&ENERCA.UMOY')
    call jedetr('&&ENERCA.UMOYZ')
    call jedetr('&&ENERCA.UPMUM')
    call jedetr('&&ENERCA.UPMUMZ')
    call jedetr('&&ENERCA.VMOY')
    call jedetr('&&ENERCA.VMOYZ')
    call jedetr('&&ENERCA.VPMVM')
    call jedetr('&&ENERCA.VPMVMZ')
!
    1001 format ('(',i3,'A1)')
    1002 format ('((A1,1X,A15,1X),',i1,'(A1,A13),A1)')
    1003 format ('((A1,1X,A15,1X),',i1,'(A1,1X,ES11.4,1X),A1)')
    call jedema()
end subroutine
