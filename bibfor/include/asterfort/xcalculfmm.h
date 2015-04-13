!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
subroutine xcalculfmm(nbno, jcalculs, jcopiels, jnodto, ndim, nodvois, &
                      jltno, jvcn, jgrlr, jbl, jbeta, jlistp , jvp, vale, &
                      deltat, levset, signls)
        integer           :: nbno                     
        integer           :: jcalculs
        integer           :: jcopiels
        integer           :: jnodto
        integer           :: ndim
        integer           :: nodvois 
        integer           :: jltno
        integer           :: jvcn
        integer           :: jgrlr
        integer           :: jbl
        integer           :: jbeta
        integer           :: jlistp
        integer           :: jvp
        real(kind=8)      :: vale(:)                                       
        real(kind=8)      :: deltat
        character(len=2)  :: levset
        character(len=3)  :: signls 
   end subroutine
end interface
