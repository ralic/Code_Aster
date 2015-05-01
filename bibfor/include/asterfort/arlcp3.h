!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine arlcp3(nbma1 ,nbma2 ,numno1,numno2,m3dea , &
                  m1dea ,numn1t,numn2t,len1  ,len2  , lisrel, charge)
       integer :: nbnomx
       parameter    (nbnomx=27)
       integer          :: nbma1,nbma2
       integer          :: len1,len2
       real(kind=8)     :: m3dea(12,3*nbnomx,nbma1),m1dea(12,12,nbma2)
       character(len=5), dimension(nbnomx+2,nbma1) :: numno1
       character(len=5), dimension(2,nbma2)  :: numno2
       character(len=5), dimension(nbnomx*nbma1) :: numn1t
       character(len=5), dimension(2*nbma2)  :: numn2t
       character(len=8) :: charge
       character(len=19) :: lisrel
    end subroutine arlcp3
end interface
