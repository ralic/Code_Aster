subroutine calc_cara_homo(noma, nomgrma, listma, nbma, ncarac,&
                           cara, vale, caram, valem)
    implicit none
    character(len=24), intent(in) :: nomgrma
    integer, intent(in) :: ncarac, listma(*), nbma
    character(len=8) , intent(in) :: cara(*), noma
    real(kind=8), intent(in) :: vale(*)
    character(len=8) , intent(out) :: caram(4)
    real(kind=8), intent(out) :: valem(*)
    
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
!   AFFE_CARA_ELEM
!   Calcul à partir des caractéristiques R_DEBUT, R_FIN, EP_DEBUT et EP_FIN
!   du groupe de mailles ordonnée, les valeurs des caractéristiques 
!   R1, R2  
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
! ----------------------------------------------------------------------
    integer, parameter :: nk = 4
    integer :: i, j, imail, numail, no1, no2
    integer :: jcxma, jcoor, i_para(nk)
    real(kind=8) :: lonpou, v_diff, vdeb, vale1, vale2, l
    real(kind=8) :: rratio, eratio, homo, valr(6)
    character(len=3) :: carpou(nk)
    character(len=8) :: carpoug(nk)
    character(len=24) :: conxma, coorno
    real(kind=8), pointer :: abs_curv(:) => null()
!
    data carpoug /'R_DEBUT', 'R_FIN', 'EP_DEBUT', 'EP_FIN'/
    data carpou /'R1', 'R2', 'EP1', 'EP2'/
!
    conxma = noma//'.CONNEX'
    coorno = noma//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    AS_ALLOCATE(vr=abs_curv, size=nbma)
!
!   remplissage de caram
    caram(:) = ''
    i_para(:) = 0
    do i=1,ncarac
        do j=1, nk
            if (cara(i) .eq. carpoug(j))then
                caram(i) = carpou(j)
                i_para(j) = i
                exit
            endif
        enddo
    enddo
    do i=1, ncarac
        ASSERT(caram(i).ne. '')
    enddo
!
!   verification que les mailles sont ordonnées et calcul de la
!   longueur de la poutre
    lonpou = 0.d0
    no1 = 0
    no2 = 0
    do imail = 1, nbma
        numail = listma(imail)
        call jeveuo(jexnum(conxma, numail), 'L', jcxma)
        if (imail .eq. 2) then
            if(no1 .eq. zi(jcxma+1))then
!               il faut réorienter toute la poutre
                call utmess('F','MODELISA5_52', sk=nomgrma)
            elseif (no2 .ne. zi(jcxma))then
!               mailles non connexes ou mailles mal orientées
                call utmess('F','MODELISA5_53', sk=nomgrma, si=imail)
            endif
        elseif (imail .ne. 1)then
            if (no2 .ne. zi(jcxma)) then
                call utmess('F', 'MODELISA5_53', sk=nomgrma, si=imail)
            endif
        endif
        no1 = zi(jcxma)
        no2 = zi(jcxma+1)
!       longueur de l'élément 
        l = sqrt( (zr(jcoor+3*(no2-1)  )-zr(jcoor+3*(no1-1)  ))**2&
                 +(zr(jcoor+3*(no2-1)+1)-zr(jcoor+3*(no1-1)+1))**2&
                 +(zr(jcoor+3*(no2-1)+2)-zr(jcoor+3*(no1-1)+2))**2)
        lonpou = lonpou + l
        abs_curv(imail) = lonpou
    end do
!
!   calcul des valeurs des paramètres pour chaque maille
    do j = 1, nk, 2
        if (i_para(j) .eq. 0) cycle
        v_diff = vale(i_para(j+1)) - vale(i_para(j))
        vdeb = vale(i_para(j))
        do imail = 1, nbma
            if (imail .eq. 1) then
                vale1 = vdeb
            else
                vale1 = vale2
            endif
            vale2 = vdeb + v_diff*abs_curv(imail)/lonpou
            valem((imail-1)*ncarac+ i_para(j)  ) = vale1
            valem((imail-1)*ncarac+ i_para(j+1)) = vale2
!           vérification du ration rayon/epaisseur
            if (j .eq. 3)then
                rratio = valem((imail-1)*ncarac+ i_para(2)) /&
                         valem((imail-1)*ncarac+ i_para(1))
                eratio = vale2 / vale1
                homo = abs((rratio - eratio) / rratio)
                if (homo .gt. 1.0d-2) then
                    valr(1) = valem((imail-1)*ncarac+ i_para(1))
                    valr(2) = valem((imail-1)*ncarac+ i_para(2))
                    valr(3) = vale1
                    valr(4) = vale2
                    valr(5) = rratio
                    valr(6) = eratio
                    call utmess('A', 'POUTRE0_4', nr=6, valr=valr)
                endif
            endif
        enddo
    enddo
!
    AS_DEALLOCATE(vr=abs_curv)
!
end subroutine
