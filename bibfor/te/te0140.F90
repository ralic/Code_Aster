subroutine te0140(option, nomte)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfrig.h"
#include "asterfort/porigi.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utpslg.h"
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
!     CALCULE LA MATRICE DE RIGIDITE ELEMENTAIRE DES ELEMENTS DE POUTRE
!     D'EULER ET DE TIMOSHENKO
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!      'RIGI_MECA '     : CALCUL DE LA MATRICE DE RIGIDITE
!      'RIGI_FLUI_STRU' : CALCUL DE LA MATRICE DE RIGIDITE (ABSC_CURV)
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!      'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!      'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!      'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!      'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!      'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!     ------------------------------------------------------------------
!
!
    integer :: imate, lmat, lorien
    integer :: nbpar, nbres, nc, nno, iret
    parameter (nbres=2)
    real(kind=8) :: valres(nbres)
    real(kind=8) :: deux, e
    real(kind=8) :: valpar, xnu, g, un, zero
    integer :: codres(nbres), kpg, spt
    character(len=8) :: nompar, fami, poum
    character(len=16) :: nomres(nbres)
    character(len=16) :: opti
    real(kind=8) :: pgl(3, 3), klv(105)
!     ------------------------------------------------------------------
    data nomres/'E','NU'/
!     ------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
!
    if (option(1:9) .eq. 'RIGI_MECA') then
        opti = 'ELAS'
    else if (option(1:14).eq.'RIGI_FLUI_STRU') then
        opti = 'ELAS_FLUI'
    else
! OPTION NON PROGRAMMEE
        ASSERT(.false.)
    endif
    if (nomte .ne. 'MECA_POU_D_TG' .and. nomte .ne. 'MECA_POU_D_TGM') then
        call moytem('NOEU', 2, 1, '+', valpar, iret)
    else
        call moytem('RIGI', 3, 1, '+', valpar, iret)
    endif
    nbpar = 1
    nompar = 'TEMP'
!
    call jevech('PMATERC', 'L', imate)
    if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
        call rcvalb(fami, kpg, spt, poum, zi(imate),' ', opti, nbpar, nompar, [valpar],&
                    nbres, nomres, valres, codres, 1)
        e = valres(1)
        xnu = valres(2)
        g = e/ (deux* (un+xnu))
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!
    klv(:) = 0.d0
!
!     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
    if ((nomte.eq.'MECA_POU_D_EM') .or. (nomte.eq.'MECA_POU_D_TGM')) then
        call pmfrig(nomte, zi(imate), klv)
    else
        call porigi(nomte, e, xnu, -1.d0, klv)
    endif
!
    call jevech('PMATUUR', 'E', lmat)
    nno = 2
    nc = 6
    if (nomte(1:13).eq.'MECA_POU_D_TG') nc = 7
!
    call matrot(zr(lorien), pgl)
    call utpslg(nno, nc, pgl, klv, zr(lmat))
!
end subroutine
