subroutine te0169(option, nomte)
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
! SUPPRESSION D'INSTRUCTIONS INUTILES
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "blas/ddot.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES FORCES NODALES DE MEPOULI
!                          REFE_FORC_NODA
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: w(9), l1(3), l2(3), forref
    real(kind=8) :: norml1, norml2, coef1, coef2
    integer :: jefint, lsigma, igeom, idepla, ideplp, ivectu, nno, nc
    integer :: ino, i, kc, iret
! ----------------------------------------------------------------------
!
    if (option .eq. 'REFE_FORC_NODA') then
        nno = 3
        nc = 3
        call terefe('EFFORT_REFE', 'MECA_POULIE', forref)
        call jevech('PVECTUR', 'E', ivectu)
        do 101 ino = 1, nno
            do 102 i = 1, nc
                zr(ivectu+(ino-1)*nc+i-1)=forref
102          continue
101      continue
!
    else if (option.eq.'FORC_NODA') then
!
!        PARAMETRES EN ENTREE
        call jevech('PGEOMER', 'L', igeom)
!
        call jevech('PDEPLMR', 'L', idepla)
        call tecach('ONO', 'PDEPLPR', 'L', iret, iad=ideplp)
        call jevech('PCONTMR', 'L', lsigma)
!        PARAMETRES EN SORTIE
        call jevech('PVECTUR', 'E', jefint)
!
        if (ideplp .eq. 0) then
            do 10 i = 1, 9
                w(i)=zr(idepla-1+i)
10          continue
        else
            do 11 i = 1, 9
                w(i)=zr(idepla-1+i)+zr(ideplp-1+i)
11          continue
        endif
!
        do 21 kc = 1, 3
            l1(kc) = w(kc )+zr(igeom-1+kc)-w(6+kc)-zr(igeom+5+kc)
21      continue
        do 22 kc = 1, 3
            l2(kc) = w(3+kc)+zr(igeom+2+kc)-w(6+kc)-zr(igeom+5+kc)
22      continue
        norml1=ddot(3,l1,1,l1,1)
        norml2=ddot(3,l2,1,l2,1)
        norml1 = sqrt (norml1)
        norml2 = sqrt (norml2)
!
        coef1 = zr(lsigma) / norml1
        coef2 = zr(lsigma) / norml2
!
        do 15 i = 1, 3
            zr(jefint+i-1) = coef1 * l1(i)
            zr(jefint+i+2) = coef2 * l2(i)
            zr(jefint+i+5) = -zr(jefint+i-1) - zr(jefint+i+2)
15      continue
    endif
end subroutine
