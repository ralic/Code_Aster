subroutine te0403(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/fcent.h"
#include "asterfort/fointe.h"
#include "asterfort/fpesa.h"
#include "asterfort/fpres.h"
#include "asterfort/fsurf.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/tecael.h"
#include "asterfort/trnflg.h"
#include "asterfort/u2mesg.h"
#include "asterfort/vectan.h"
    character(len=16) :: option, nomte
!
!
    integer :: nb1
    real(kind=8) :: vecl(51)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectt(9, 2, 3), vectpt(9, 3, 3)
    real(kind=8) :: valpar(4), pr
    character(len=8) :: nompar(4), nomail
    character(len=24) :: valk
! DEB ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iadzi, iazk24, ib, ier, itemps, j
    integer :: jgeom, jpres, jvecg, lzi, lzr, nb2
!-----------------------------------------------------------------------
    call jevech('PGEOMER', 'L', jgeom)
!
    call jevech('PVECTUR', 'E', jvecg)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    call vectan(nb1, nb2, zr(jgeom), zr(lzr), vecta,&
                vectn, vectt)
!
    do 5 ib = 1, nb2
        do 6 i = 1, 2
            do 7 j = 1, 3
                vectpt(ib,i,j)=vectt(ib,i,j)
 7          end do
 6      end do
        vectpt(ib,3,1)=vectn(ib,1)
        vectpt(ib,3,2)=vectn(ib,2)
        vectpt(ib,3,3)=vectn(ib,3)
 5  end do
!
!
    if (option .eq. 'CHAR_MECA_FRCO3D' .or. option .eq. 'CHAR_MECA_FFCO3D') then
!------------------------------------------------------
!      PAS DE CHANGEMENT DE SIGNE POUR LES FORCES REPARTIES
!------------------------------------------------------
        call fsurf(option, nomte, zr(jgeom), nb1, vecl,&
                   vectpt)
!
    else if (option.eq.'CHAR_MECA_PESA_R') then
        call fpesa(nomte, zr(jgeom), nb1, vecl)
!
    else if (option.eq.'CHAR_MECA_ROTA_R') then
        call fcent(nomte, zr(jgeom), nb1, vecl)
!
    else if (option.eq.'CHAR_MECA_PRES_R') then
!------------------------------------------------------
!      CHANGEMENT DE SIGNE POUR LES PRESSIONS DANS FPRES
!------------------------------------------------------
        call fpres(nomte, zr(jgeom), nb1, vecl, vectpt)
!
    else if (option.eq.'CHAR_MECA_PRES_F') then
        call jevech('PPRESSF', 'L', jpres)
        if (zk8(jpres) .eq. '&FOZERO') goto 9999
        call jevech('PTEMPSR', 'L', itemps)
        valpar(4) = zr(itemps)
        nompar(4) = 'INST'
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        do 222 j = 0, nb1-1
            valpar(1) = zr(jgeom+3*j )
            valpar(2) = zr(jgeom+3*j+1)
            valpar(3) = zr(jgeom+3*j+2)
            call fointe('FM', zk8(jpres), 4, nompar, valpar,&
                        pr, ier)
            if (pr .ne. 0.d0) then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3)(1:8)
                valk = nomail
                call u2mesg('F', 'ELEMENTS4_92', 1, valk, 0,&
                            0, 0, 0.d0)
            endif
222      continue
        goto 9999
    endif
!
    call trnflg(nb2, vectpt, vecl, zr(jvecg))
!
9999  continue
end subroutine
