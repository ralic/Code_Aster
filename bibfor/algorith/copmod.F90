subroutine copmod(basemo, champ, neq, numer, nbmode,&
                  typc, bmodr, bmodz)
    implicit none
!***********************************************************************
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
!
!                              FONCTION
!     _______________________________________________________________
!    | EXTRAIRE, DANS UN VECTEUR DE TRAVAIL, DES CHAMPS D'UN CONCEPT |
!    | MODE_MECA AVEC LA POSSIBILITE DE MODIFIER LA NUMEROTATION.    |
!    |_______________________________________________________________|
!
! ---------
! EXEMPLES: CALL COPMOD (BASE,'DEPL',NEQ,' '  ,NBMOD,'R',ZR(JBASE),CBID)
! --------- CALL COPMOD (BASE,'DEPL',NEQ,NUDDL,NBMOD,'R',ZR(JBASE),CBID)
!           CALL COPMOD (BASE,'DEPL',NEQ,NUDDL,NBMOD,'C',RBID,ZC(JBASE))
!
!
!                     DESCRIPTIVE DES VARIABLES
!   ___________________________________________________________________
!  | IN > BASEMO : BASE MODALE D'ENTREE (MODE_MECA)                [K8]|
!  | IN > CHAMP  : NOM DE CHAMP A EXTRAIRE (EX. 'DEPL')            [K*]|
!  | IN > NEQ    : NOMBRE D'EQUATIONS DANS LE SYSTEME ASSEMBLE      [I]|
!   ___________________________________________________________________
!  | IN > NUMER  :                                                 [K*]|
!  |        SOIT - BLANC (' ') SI ON VEUT CONSERVER LA NUMEROTATION    |
!  |               INITIALE                                            |
!  |        SOIT - LE NOM DE CONCEPT NUME_DDL OU SD_PROF_CHNO DONNANT  |
!  |               LA NOUVELLE NUMEROTATION DES CHAMPS                 |
!   ___________________________________________________________________
!  | IN > NBMODE : NOMBRE DE MODES DANS BASEMO                      [I]|
!  | IN > TYPC   : TYPE DES CHAMPS A COPIER ('R'/'C')              [K1]|
!   ___________________________________________________________________
!  |OUT < BMODR  : VECTEUR DE TRAVAIL (REEL) DE SORTIE             [R8]|
!  |OUT < BMODZ  : VECTEUR DE TRAVAIL (COMPLEXE) DE SORTIE        [C16]|
!   ___________________________________________________________________
!
#include "jeveux.h"
!   ___________________________________________________________________
!
!  - 0 - INITIALISATIONS DIVERSES
!   ___________________________________________________________________
!
!     0.1 - DECLARATION DES VARIABLES D'ENTREE/SORTIE
!
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcrea.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/zcopy.h"
    character(len=1) :: typc
    character(len=8) :: basemo
    character(len=*) :: numer
    character(len=*) :: champ
    integer :: neq, nbmode
    real(kind=8) :: bmodr(neq*nbmode)
    complex(kind=8) :: bmodz(neq*nbmode)
!
!     0.2 - DECLARATION DES VARIABLES LOCALES
!
    logical :: modnum, exnume
    integer :: i, ib, iret
    integer :: jref, jdeeq, jval
    character(len=19) :: numer1, numer2, nomcha, tmpcha
    character(len=24) :: maill1, maill2, valk(4), crefe(2), valcha
!
!     0.3 - ACTUALISATION DE LA VALEUR DE LA MARQUE COURANTE
!
    call jemarq()
!  ____________________________________________________________________
!
!  - 1 - RECHERCHE DES INFORMATIONS SUR LES CHAMPS DANS LA BASE MODALE
!  ____________________________________________________________________
!
!     1.1 - CHERCHER UN OBJET .REFE DANS UN CHAMP DE LA BASE MODALE
!
!     1.1.1 - RECUPERER LE NOM DE CHAMP DU 1ER NUMERO ORDRE
!
    call rsexch('F', basemo, champ, 1, nomcha,&
                iret)
!
!     1.1.2 - POUR TRAITER LES CAS AVEC SS-STRUCTURATION, TESTER SI
!             L'OBJET .REFE EXISTE DANS CE CHAMP, SI NON (IRET.EQ.0)
!             RECUPERER LE .REFE DU CHAMP DE DEPLACEMENT
!
    call jeexin(nomcha(1:19)//'.REFE', iret)
    if (iret .eq. 0) call rsexch('F', basemo, 'DEPL', 1, nomcha,&
                                 iret)
!
    call jeveuo(nomcha(1:19)//'.REFE', 'L', jref)
!
!     1.2 - EXTRAIRE LE NOM DE MAILLAGE .REFE[1] ET DU NUME_DDL .REFE[2]
!
    maill1 = zk24(jref)(1:8)
    numer1 = zk24(jref+1)(1:19)
!
!     1.3 - TRAITEMENT DES CAS AVEC UN PROF_CHNO ET NON PAS UN NUME_DDL
!           COMPLET.
!
    exnume = .false.
    numer2 = numer
    if (numer1(15:15) .eq. ' ') numer1 = numer1(1:14)//'.NUME'
    if (numer2(15:15) .eq. ' ') then
        exnume = .true.
        numer2 = numer2(1:14)//'.NUME'
    else
!       --- ON NE FAIT PAS DE TEST DE COMPATIBILITE SUR LES MAILLAGES
!         - SI ON NE DISPOSE PAS DE NUMEDDL COMPLET
!         - IMPORTANT : LE TEST DOIT SE FAIRE QUAND MEME EN DEHORS DE
!                       L'APPEL A COPMOD (VOIR OP0072 PAR EXEMPLE)
        maill2 = maill1
    endif
!
!     1.4 - LIBERER L'OBJET .REFE PARCE QU'ON N'EN A PLUS BESOIN
!
    call jelibe(nomcha(1:19)//'.REFE')
!  ____________________________________________________________________
!
!  - 2 - RECHERCHE DES INFORMATIONS SUR LA NUMEROTATION FINALE
!  ____________________________________________________________________
!
!     2.1 - NOUVELLE NUMEROTATION ? (SUR UN UNIQUE MAILLAGE)
!     2.2 - SI OUI, VERIFIER LA COMPATIB. DES 2 MAILLAGES DES NUME_DDL
!           RESTITUTION SUR SQUELLETE : CAS SPECIAL
!
    call jeexin(maill1(1:8)//'.INV.SKELETON', iret)
    modnum = .false.
    if (numer2 .ne. ' ') then
        if ((numer2.ne.numer1) .and. (iret.eq.0) .and. (exnume)) then
            call dismoi('F', 'NOM_MAILLA', numer2(1:14), 'NUME_DDL', ib,&
                        maill2, iret)
            if (maill1 .ne. maill2) then
                valk (1) = numer2
                valk (2) = maill2
                valk (3) = numer1
                valk (4) = maill1
                call u2mesg('F', 'ALGORITH12_62', 4, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
        if ((numer2.ne.numer1) .and. (iret.eq.0)) then
            modnum = .true.
        endif
    endif
!
!
!     2.3 - RECUPERER L'OBJET .DEEQ
!
    if (modnum) then
        call jeveuo(numer2//'.DEEQ', 'L', jdeeq)
    else
        call jeveuo(numer1//'.DEEQ', 'L', jdeeq)
    endif
!  ____________________________________________________________________
!
!  - 3 - RECOPIE DES CHAMPS ET MODIFICATION DE LA NUMER. SI NECESSAIRE
!  ____________________________________________________________________
!
!     3.1 - BOUCLE SUR LES MODES DE LA BASE
    do 10 i = 1, nbmode
!       3.1.1 - EXTRAIRE LE NOM DU CHAMP D'INTERET (NOMCHA)
        call rsexch('F', basemo, champ, i, nomcha,&
                    iret)
!
!       3.1.2 - NOUVELLE NUMER.? ALORS CREER UN NOUVEAU CHAMP TEMPORAIRE
!               AVEC LA BONNE NUMEROTATION
        if (modnum) then
            crefe(1) = maill2
            crefe(2) = numer2
            tmpcha = '&&COPMOD.CHAMP'
            call vtcrea(tmpcha, crefe, 'V', typc, neq)
            call vtcopy(nomcha, tmpcha, ' ', iret)
            if (iret .ne. 0) then
                valk(1) = nomcha
                valk(2) = tmpcha
                valk(3) = crefe(2)(1:8)
                call u2mesk('A', 'UTILITAI_24', 3, valk)
            endif
            nomcha = tmpcha
        endif
!
!       3.1.3 - OBTENIR L'OBJET DES VALEURS DU CHAMP (.VALE OU .CELV)
!               POUR LES CHAM_NO ET CHAM_ELEM RESPECTIVEMENT
        valcha = nomcha(1:19)//'.VALE'
        call jeexin(nomcha(1:19)//'.VALE', iret)
        if (iret .le. 0) valcha = nomcha(1:19)//'.CELV'
        call jeveuo(valcha, 'L', jval)
!
!       3.1.4 - COPIER LES VALEURS DU CHAMP DANS LE VECTEUR DE SORTIE
        if (typc .ne. 'C') then
            call dcopy(neq, zr(jval), 1, bmodr((i-1)*neq+1), 1)
        else
            call zcopy(neq, zc(jval), 1, bmodz((i-1)*neq+1), 1)
        endif
!
!       3.1.5 - MENAGE ET LIBERATION DE LA MEMOIRE SELON LE BESOIN
        call jelibe(valcha)
        if (modnum) then
            if (valcha(21:24) .eq. 'VALE') then
                call detrsd('CHAM_NO', tmpcha)
            else
                call detrsd('CHAM_ELEM', tmpcha)
            endif
        endif
!
!       3.1.6 - ANNULER LES DDL DE LAGRANGE S'IL S'AGIT DES CHAMPS DE
!               DEPLACEMENTS
        if (champ .eq. 'DEPL') then
            call zerlag(typc, bmodr((i-1)*neq+1), bmodz((i-1)*neq+1), neq, zi(jdeeq))
        endif
!
10  end do
!     FIN DE LA BOUCLE (3.1) SUR LES MODES
!  ____________________________________________________________________
!
    call jedema()
end subroutine
