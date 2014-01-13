subroutine copmod(base, bmodr, bmodz, champ, numer, &
                  nbmodes, nequa)
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
!                              Function
!     _____________________________________________________________
!    | Extract, inside a temporary work vector, the fields from a  |
!    | modal basis concept while changing or not their numbering   |
!    |_____________________________________________________________|
!
! ---------
! Examples: call copmod ( base, bmodr = zr(jbase) )
! --------- call copmod ( base, bmodr = zr(jbase), numer = nuddl )
!           call copmod ( base, bmodz = zc(jbase) )
!           call copmod ( base, bmodr = zr(jbase), numer = nuddl, champ = 'VITE')
!
!
!                     Description of the input/output arguments
!   _________________________________________________________________________________
!  | in < obl > base      : Entry modal basis (mode_meca)                        [k8]|
!  |            ------                                                               |
!  |out < fac >|bmodr |   : Output work vector containing the copied fields      [r8]|
!  |            ======    : (real fields case)                                       |
!  |out < fac >|bmodz |   : Output work vector containing the copied fields     [c16]|
!  |            ------|   : (complex fields case)                                    |
!  |                  |                                                              |
!  |                   => Validation rule : One of these two output vectors must     |
!  |                                        be given                                 |
!  |---------------------------------------------------------------------------------|
!  | in < fac > champ     : Field type to copy (default = 'DEPL')                [k*]|
!  | in < fac > numer     : - If given, the name of the nume_ddl concept         [k*]|
!  |                      :   or that of the prof_chno giving the new numbering      |
!  |                      :   of the copied fields                                   |
!  |                      : - If absent, the numbering is unchanged                  |
!  | in < fac > nbmodes   : The number of modes to be copied                      [i]|
!  |                      : (default = total number of modes in base)              |
!  | in < fac > nequa     : The number of equations in each vector                [i]|
!  |                      : (default = determine automatically from the nume_ddl)    |
!  |_________________________________________________________________________________|
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcrea.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/zcopy.h"
!   ___________________________________________________________________
!
!  - 0 - INITIALISATIONS DIVERSES
!   ___________________________________________________________________
!
!     0.1 - DECLARATION DES VARIABLES D'ENTREE/SORTIE
!
    character(len=8), intent(in):: base
    real(kind=8), optional, intent(out) :: bmodr(*)
    complex(kind=8), optional, intent(out) :: bmodz(*)
    character(len=*), optional, intent(in) :: champ
    character(len=*), optional, intent(in) :: numer
    integer, optional, intent(in) :: nbmodes
    integer, optional, intent(in) :: nequa
!
!     0.2 - DECLARATION DES VARIABLES LOCALES
!
    character(len=1) :: typc
    logical :: modnum, exnume
    integer :: i, iret, neq, nbmode
    integer :: jref, jdeeq, jval
    character(len=8) :: champ2
    character(len=19) :: numer1, numer2, nomcha, tmpcha
    character(len=24) :: maill1, maill2, valk(4), crefe(2), valcha
!
!     0.3 - ACTUALISATION DE LA VALEUR DE LA MARQUE COURANTE
!
    call jemarq()
!
!     0.4 - TEST DES ARGUMENTS D'ENTREES, ET ATTRIBUTION DES VALEURS PAR DEFAUT
!
    typc = 'R'
    ASSERT(UN_PARMI2(bmodr, bmodz))
    if (present(bmodz)) typc = 'C'

    champ2  = 'DEPL'
    numer2  = ' '
    if (present(champ)) champ2 = champ
    if (present(numer)) numer2 = numer
    if (present(nequa)) neq = nequa

!   --- VERIFICATION QUE LE NOMBRE D'EQUATIONS RENSEIGNE CORRESPOND AU NUME_DDL
    if (numer2 .ne. ' ') then
        call jeexin(numer2(1:14)//'.NUME.NEQU', iret)
        if (iret .ne. 0) then
            call dismoi('NB_EQUA' , numer2, 'NUME_DDL' , repi=neq)
            if (present(nequa)) ASSERT(nequa .eq. neq)
        endif
    else 
        call dismoi('NUME_DDL', base, 'RESU_DYNA', repk=numer1, arret='C', ier=iret)
        if (iret .eq. 0) then
            call jeexin(numer1(1:14)//'.NUME.NEQU', iret)
            if (iret .ne. 0) then
                call dismoi('NB_EQUA' , numer1, 'NUME_DDL' , repi=neq)
                if (present(nequa)) ASSERT(nequa .eq. neq)
            endif
        endif
    endif
!   --- FIN DE LA VERIFICATION DU NOMBRE D'EQUATIONS

    call dismoi('NB_MODES_TOT', base, 'RESULTAT' , repi=nbmode)
    if (present(nbmodes)) then 
        ASSERT(nbmodes .le. nbmode)
        nbmode = nbmodes
    endif


!  ____________________________________________________________________
!
!  - 1 - RECHERCHE DES INFORMATIONS SUR LES CHAMPS DANS LA BASE MODALE
!  ____________________________________________________________________
!
!     1.1 - CHERCHER UN OBJET .REFE DANS UN CHAMP DE LA BASE MODALE
!
!     1.1.1 - RECUPERER LE NOM DE CHAMP DU 1ER NUMERO ORDRE
!
    call rsexch('F', base, champ2, 1, nomcha,&
                iret)
!
!     1.1.2 - POUR TRAITER LES CAS AVEC SS-STRUCTURATION, TESTER SI
!             L'OBJET .REFE EXISTE DANS CE CHAMP, SI NON (IRET.EQ.0)
!             RECUPERER LE .REFE DU CHAMP DE DEPLACEMENT
!
    call jeexin(nomcha(1:19)//'.REFE', iret)
    if (iret .eq. 0) call rsexch('F', base, 'DEPL', 1, nomcha,&
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
            call dismoi('NOM_MAILLA', numer2(1:14), 'NUME_DDL', repk=maill2)
            if (maill1 .ne. maill2) then
                valk (1) = numer2
                valk (2) = maill2
                valk (3) = numer1
                valk (4) = maill1
                call utmess('F', 'ALGORITH12_62', nk=4, valk=valk)
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
    do i = 1, nbmode
!       3.1.1 - EXTRAIRE LE NOM DU CHAMP D'INTERET (NOMCHA)
        call rsexch('F', base, champ2, i, nomcha,&
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
                call utmess('A', 'UTILITAI_24', nk=3, valk=valk)
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
        if (champ2 .eq. 'DEPL') then
            if (typc .ne. 'C') then
                call zerlag(neq, zi(jdeeq), vectr=bmodr((i-1)*neq+1))
            else
                call zerlag(neq, zi(jdeeq), vectz=bmodz((i-1)*neq+1))
            endif
        endif
!
    end do
!     FIN DE LA BOUCLE (3.1) SUR LES MODES
!  ____________________________________________________________________
!
    call jedema()
end subroutine
