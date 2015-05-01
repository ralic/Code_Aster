subroutine affori(typ, nomt, cara, val, jad, jin, &
                  jdno, jdco, nutyma, ntseg, &
                  lseuil, nbseuil)
!
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
    integer :: nutyma, ntseg, jad, jin, jdno, jdco
    character(len=*) :: typ, nomt, cara
    real(kind=8) :: val(6)
    real(kind=8), intent(in), optional :: lseuil
    integer, intent(inout), optional :: nbseuil
!
! --------------------------------------------------------------------------------------------------
!
!   AFFECTATION DES ORIENTATIONS AUX POI1 ET SEG2 POSSIBLES DANS LE VECTEUR TAMPON TMPORI
!
! --------------------------------------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/angvxy.h"
#include "asterfort/utmess.h"
#include "asterfort/vdiff.h"
#include "blas/ddot.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer ::  no1, no2, ii
    character(len=16) :: affcar, nom
    character(len=24) :: vmessk(2)
    real(kind=8) :: x1(3), x2(3), x3(3), angl(3), seglong, segseuil
    real(kind=8) :: alpha, beta, gamma
    aster_logical :: sousseuil,longnulle
!
! --------------------------------------------------------------------------------------------------
!
    affcar = cara
    nom = nomt
!   Seuil pour les longueurs
!       si seglong .LT. segseuil ==> maille est considérée de taille nulle
    segseuil = -1.0d0
    if ( present(lseuil) ) then
        segseuil = lseuil
    endif
!
! --------------------------------------------------------------------------------------------------
!   calcul de la longueur du segment
    seglong = 0.0d0
    longnulle = ASTER_TRUE
    sousseuil = ASTER_TRUE
    if (typ(1:6) .eq. 'MAILLE') then
        if (nutyma .eq. ntseg) then
            no1 = zi(jdno)
            no2 = zi(jdno+1)
            do ii = 1, 3
                x1(ii) = zr(jdco+(no1-1)*3+ii-1)
                x2(ii) = zr(jdco+(no2-1)*3+ii-1)
            enddo
            call vdiff(3, x2, x1, x3)
            seglong = sqrt( ddot(3,x3,1,x3,1) )
            if ( seglong .gt. 0.0d0 ) then
                longnulle = ASTER_FALSE
            else
                longnulle = ASTER_TRUE
            endif
            if ( seglong .gt. segseuil ) then
                sousseuil = ASTER_FALSE
            else
                sousseuil = ASTER_TRUE
            endif
        endif
    endif
!
! --------------------------------------------------------------------------------------------------
    vmessk(1) = affcar
    vmessk(2) = nom
! --------------------------------------------------------------------------------------------------
    if (affcar .eq. 'ANGL_VRIL') then
        gamma = r8dgrd() * val(1)
        if (typ(1:6) .eq. 'MAILLE') then
!           Si MAILLE : si ce n'est pas un SEG2 <F>
            if (nutyma .ne. ntseg) then
                call utmess('F', 'MODELISA_87', nk=2, valk=vmessk)
            endif
!           si longueur(SEG2)=0 ou sous le seuil <F>
            if (longnulle.or.sousseuil) then
                call utmess('F', 'MODELISA_88', nk=2, valk=vmessk)
            endif
!           Impression message si surcharge
            call messsurcharge(affcar,nom,'gamma',zi(jin+2),zr(jad+2),gamma)
!           Affectation de gamma
            zr(jad+2) = gamma
            zi(jin+2) = zi(jin+2) + 1
        else
!           Noeud : pas d'affectation possible sur un noeud <F>
            call utmess('F', 'MODELISA_89', nk=2, valk=vmessk)
        endif
! --------------------------------------------------------------------------------------------------
    else if (affcar .eq. 'ANGL_NAUT') then
        alpha = r8dgrd() * val(1)
        beta  = r8dgrd() * val(2)
        gamma = r8dgrd() * val(3)
        if (typ(1:6) .eq. 'MAILLE') then
!           Si MAILLE : si longueur(SEG2)<>0 et au-dessus du seuil <F>
            if ((.not. longnulle).and.(.not. sousseuil)) then
                call utmess('F', 'MODELISA_90', nk=2, valk=vmessk, sr=seglong)
            endif
!           si longueur(SEG2)=0 ou sous le seuil
            if ( present(nbseuil).and.sousseuil ) then
                nbseuil = nbseuil+1
            endif
!           Impression message si surcharge
            call messsurcharge(affcar,nom,'alpha',zi(jin),  zr(jad),  alpha)
            call messsurcharge(affcar,nom,'beta', zi(jin+1),zr(jad+1),beta )
            call messsurcharge(affcar,nom,'gamma',zi(jin+2),zr(jad+2),gamma)
!           Affectation des angles
            zr(jad)   = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            zi(jin  ) = zi(jin  ) + 1
            zi(jin+1) = zi(jin+1) + 1
            zi(jin+2) = zi(jin+2) + 1
        else
!           Impression message si surcharge
            call messsurcharge(affcar,nom,'alpha',zi(jin),  zr(jad),  alpha)
            call messsurcharge(affcar,nom,'beta', zi(jin+1),zr(jad+1),beta )
            call messsurcharge(affcar,nom,'gamma',zi(jin+2),zr(jad+2),gamma)
!           Affectation des angles
            zr(jad)   = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            zi(jin  ) = zi(jin  ) + 1
            zi(jin+1) = zi(jin+1) + 1
            zi(jin+2) = zi(jin+2) + 1
        endif
!
! --------------------------------------------------------------------------------------------------
    else if (affcar .eq. 'VECT_X_Y') then
        if (typ(1:6) .eq. 'MAILLE') then
!           Si MAILLE : si longueur(SEG2)<>0 et au-dessus du seuil <F>
            if ((.not. longnulle).and.(.not. sousseuil)) then
                call utmess('F', 'MODELISA_90', nk=2, valk=vmessk, sr=seglong)
            endif
!           si longueur(SEG2)=0 ou sous le seuil
            if ( present(nbseuil).and.sousseuil ) then
                nbseuil = nbseuil+1
            endif
            call angvxy(val(1), val(4), angl)
            alpha = angl(1)
            beta  = angl(2)
            gamma = angl(3)
!           Impression message si surcharge
            call messsurcharge(affcar,nom,'alpha',zi(jin),  zr(jad),  alpha)
            call messsurcharge(affcar,nom,'beta', zi(jin+1),zr(jad+1),beta )
            call messsurcharge(affcar,nom,'gamma',zi(jin+2),zr(jad+2),gamma)
!           Affectation des 3 angles
            zr(jad)   = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            zi(jin  ) = zi(jin  ) + 1
            zi(jin+1) = zi(jin+1) + 1
            zi(jin+2) = zi(jin+2) + 1
        else
!           Si (POI1) : affectation des 3 angles
            call angvxy(val(1), val(4), angl)
            alpha = angl(1)
            beta  = angl(2)
            gamma = angl(3)
!           Impression message si surcharge
            call messsurcharge(affcar,nom,'alpha',zi(jin),  zr(jad),  alpha)
            call messsurcharge(affcar,nom,'beta', zi(jin+1),zr(jad+1),beta )
            call messsurcharge(affcar,nom,'gamma',zi(jin+2),zr(jad+2),gamma)
            zr(jad)   = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            zi(jin  ) = zi(jin  ) + 1
            zi(jin+1) = zi(jin+1) + 1
            zi(jin+2) = zi(jin+2) + 1
        endif
! --------------------------------------------------------------------------------------------------
    else if (affcar .eq. 'VECT_Y') then
        if (typ(1:6) .eq. 'MAILLE') then
!           Si Maille : si ce n'est pas un SEG2 <F>
            if (nutyma .ne. ntseg) then
                call utmess('F', 'MODELISA_91', nk=2, valk=vmessk)
            endif
!           si longueur(SEG2)=0
            if (longnulle.or.sousseuil) then
                call utmess('F', 'MODELISA_88', nk=2, valk=vmessk)
            endif
!           si longueur(SEG2)<>0
            call angvxy(x3, val(1), angl)
            gamma = angl(3)
!           Impression message si surcharge
            call messsurcharge(affcar,nom,'gamma',zi(jin+2),zr(jad+2),gamma)
!           Affectation de gamma
            zr(jad+2) = gamma
            zi(jin+2) = zi(jin+2) + 1
        else
!           Noeud : pas d'affectation sur un POI1 <F>
            call utmess('F', 'MODELISA_89', nk=2, valk=vmessk)
        endif
    else
        ASSERT( ASTER_FALSE )
    endif
!
! ==================================================================================================
contains
subroutine messsurcharge(kk1,kk2,kk3,ii1,rr1,rr2)
    character(len=*) :: kk1,kk2,kk3
    integer :: ii1
    real(kind=8) :: rr1,rr2
!
#include "asterfort/utmess.h"
#include "asterc/r8rddg.h"
!
    character(len=24) :: vmessk(3)
    real(kind=8) :: vmessr(2)
!
    if ((abs(rr1-rr2).gt.r8prem()).and.(ii1.ne.0)) then
        vmessk(1) = kk1
        vmessk(2) = kk2
        vmessk(3) = kk3
        vmessr(1) = rr1*r8rddg()
        vmessr(2) = rr2*r8rddg()
        call utmess('A', 'MODELISA2_7', nk=3, valk=vmessk, nr=2, valr=vmessr)
    endif
end subroutine messsurcharge
!
end subroutine affori
