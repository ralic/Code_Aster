subroutine mereso(result, modele, mate, carele, fomult,&
                  lischa, itps, partps, numedd, vecass,&
                  assmat, solveu, matass, maprec, base,&
                  compor)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     MECANIQUE STATIQUE - RESOLUTION
!     * *                  ****
! ----------------------------------------------------------------------
!     BUT:  FAIRE UN CALCUL DE MECANIQUE STATIQUE : K(T)*U = F(T)
!           A UN INSTANT DONNE
! ----------------------------------------------------------------------
! IN  MODELE  : NOM DU MODELE
! IN  MATE    : NOM DU MATERIAU
! IN  CARELE  : NOM D'1 CARAC_ELEM
! IN  FOMULT  : LISTE DES FONCTIONS MULTIPLICATRICES
! IN  LISCHA  : INFORMATION SUR LES CHARGEMENTS
! IN  ITPS    : NUMERO DU PAS DE TEMPS
! IN  PARTPS  : PARAMETRES TEMPORELS
! IN  NUMEDD  : PROFIL DE LA MATRICE
! IN  VECASS  : SECOND MEMBRE ASSEMBLE
! IN  ASSMAT  : BOOLEEN POUR LE CALCUL DE LA MATRICE
! IN  SOLVEU  : METHODE DE RESOLUTION 'LDLT' OU 'GCPC'
! IN/OUT  MAPREC  : MATRICE PRECONDITIONNEE
! IN  BASE    : BASE DE TRAVAIL
! IN  COMPOR : COMPOR POUR LES MULTIFIBRE (POU_D_EM)
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infniv.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/meacmv.h"
#include "asterfort/resoud.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/uttcpu.h"
    integer :: itps
    aster_logical :: assmat
    character(len=1) :: base
    character(len=19) :: lischa, solveu
    character(len=19) :: vecass
    character(len=19) :: matass, maprec
    character(len=24) :: modele, carele, fomult
    character(len=24) :: numedd, compor
    character(len=*) :: mate
    character(len=8) :: result
!
    real(kind=8) :: partps(3)
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'MERESO' )
    integer :: jcrk
    integer :: jpara, iainst
    integer :: iret
    integer :: ifm, niv
    character(len=8) :: k8bid
    character(len=19) :: chdepl, chsol
    character(len=24) :: cnchci
    character(len=24) :: criter
    character(len=24) :: depl
    complex(kind=8) :: cbid
    integer, pointer :: crti(:) => null()
    real(kind=8), pointer :: crtr(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!
! DEB-------------------------------------------------------------------
!====
! 1. PREALABLES
!====
!
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
!
! 1.2. ==> NOM DES STRUCTURES
!
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
    chsol = '&&'//nompro//'_SOLUTION  '
    criter = '&&'//nompro//'_RESGRA_GCPC    '
!
    depl = '&&MERESO.DEPL'
!
!
!====
! 3. MATRICE ET SECOND MEMBRE
!====
!
    call meacmv(modele, mate, carele, fomult, lischa,&
                partps, numedd, assmat, solveu, vecass,&
                matass, maprec, cnchci, base, compor)
!
!====
! 4. RESOLUTION AVEC VECASS COMME SECOND MEMBRE
!====
!
    call resoud(matass, maprec, solveu, cnchci, 0,&
                vecass, chsol, 'V', [0.d0], [cbid],&
                criter, .true._1, 0, iret)
!
! 5.1. ==> NETTOYAGE DU CHAMP CINEMATIQUE CNCHCI QUI EST RECREE A
!          CHAQUE FOIS
    call detrsd('CHAMP_GD', cnchci)
!
!====
! 5. SAUVEGARDE DE LA SOLUTION
!====
!
! 5.1. ==> SAUVEGARDE DU CHAMP SOLUTION CHSOL DANS DEPL
    call copisd('CHAMP_GD', 'V', chsol(1:19), depl(1:19))
!
! 5.2. ==> DESTRUCTION DU CHAMP SOLUTION CHSOL
!
    call detrsd('CHAMP_GD', chsol)
!
! 5.3. ==> STOCKAGE DE LA SOLUTION, DEPL, DANS LA STRUCTURE DE RESULTAT
!          EN TANT QUE CHAMP DE DEPLACEMENT A L'INSTANT COURANT
!
    write (ifm,100) 'DEPL', partps(1), itps
!
    call rsexch(' ', result, 'DEPL', itps, chdepl,&
                iret)
!
    if (iret .le. 100) then
        call copisd('CHAMP_GD', 'G', depl(1:19), chdepl(1:19))
        call rsnoch(result, 'DEPL', itps)
    endif
!
!
!*** INST
!
    call rsadpa(result, 'E', 1, 'INST', itps,&
                0, sjv=iainst, styp=k8bid)
    zr(iainst) = partps(1)
!
!*** METHODE, RENUM, ...
!
    call rsadpa(result, 'E', 1, 'METHODE', itps,&
                0, sjv=jpara, styp=k8bid)
    zk16(jpara) = slvk(1)(1:16)
    call rsadpa(result, 'E', 1, 'RENUM', itps,&
                0, sjv=jpara, styp=k8bid)
    zk16(jpara) = slvk(4)(1:16)
    call rsadpa(result, 'E', 1, 'STOCKAGE', itps,&
                0, sjv=jpara, styp=k8bid)
    if (slvk(1)(1:4) .eq. 'LDLT') then
        zk16(jpara) = 'LIGN_CIEL'
    else
        zk16(jpara) = 'MORSE'
    endif
!
!*** LES CRITERES
!
    call jeexin(criter(1:19)//'.CRTI', iret)
    if (iret .ne. 0) then
        call jeveuo(criter(1:19)//'.CRTI', 'L', vi=crti)
        call jeveuo(criter(1:19)//'.CRTR', 'L', vr=crtr)
        call jeveuo(criter(1:19)//'.CRDE', 'L', jcrk)
        call rsadpa(result, 'E', 1, zk16(jcrk), itps,&
                    0, sjv=jpara, styp=k8bid)
        zi(jpara) = crti(1)
        call rsadpa(result, 'E', 1, zk16(jcrk+1), itps,&
                    0, sjv=jpara, styp=k8bid)
        zr(jpara) = crtr(1)
    endif
    call uttcpu('CPU.OP0046.3', 'FIN', ' ')
!
    100 format(1p,3x,'CHAMP STOCKE : ',a16,' INSTANT : ',1pe12.5,&
     &         '  NUMERO D''ORDRE : ',i5)
!
end subroutine
