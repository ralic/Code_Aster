subroutine arlcp3(nbma1 ,nbma2 ,numno1,numno2,m3dea , &
                  m1dea ,numn1t,numn2t,len1  ,len2  , lisrel, charge)

! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! ASSEMBLAGE DANS LES MATRICES ELEMENTAIRES DE COUPLAGE ARLEQUIN

! ----------------------------------------------------------------------

    implicit none

#include "asterfort/jemarq.h"
#include "asterfort/afrela.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------

    integer :: nbnomx
    parameter    (nbnomx=27)
    integer          :: nbma1,nbma2
    integer          :: len1,len2
    real(kind=8)     :: m3dea(12,3*nbnomx,nbma1),m1dea(12,12,nbma2)
    character(len=19) :: lisrel
    character(len=8) :: charge
    character(len=5), dimension(nbnomx+2,nbma1) :: numno1
    character(len=5), dimension(2,nbma2)  :: numno2
    character(len=5), dimension(nbnomx*nbma1) :: numn1t
    character(len=5), dimension(2*nbma2)  :: numn2t

!-----------------------------------------------------------------------
    integer          :: nbterm
    complex(kind=8)  :: betac,coefc(3*len1+6*len2)
    character(len=2) :: typlag
    character(len=4) :: typcoe,typval
    integer          :: jdime(3*len1+6*len2,3)
    real(kind=8)     :: beta,direct(3*len1+6*len2,3)
    real(kind=8)     :: coefri(3*len1+6*len2)
    integer          :: i,j,k,m,n,p,q
    real(kind=8)     :: m1dass(6*len2,6*len2),m3dass(6*len2,3*len1)
    character(len=8) :: ddl1(3*len1+6*len2)
    real(kind=8)     :: mmixe1(6*len2,3*len1+6*len2)
    character(len=8) :: noeud1(3*len1+6*len2)
!-----------------------------------------------------------------------
    call jemarq()

! --- INITIALISATIONS

    m1dass = 0.0
    m3dass = 0.0
    mmixe1 = 0.0

! --- ASSEMBLAGE 1D

    do 81 i= 1,len2
       do 82 j= 1,len2
          do 83 k= 1,nbma2
             do 84 m=1,2
                if (numno2(m,k) == numn2t(i)) then
                   do 85 n=1,2
                      if (numno2(n,k) == numn2t(j)) then  
                         do 86 p=1,6
                            do 87 q=1,6
                               m1dass(6*(i-1)+p,6*(j-1)+q)= m1dass(6*(i-1)+p,6*(j-1)+q) &
                                             + m1dea(6*(m-1)+p,6*(n-1)+q,k)
                            87 end do
                         86 end do
                      endif
                   85 end do
                endif
             84 end do
          83 end do
       82 end do
    81 end do

! --- ASSEMBLAGE 3D

    do 181 i= 1,len2
       do 182 j= 1,len1
          do 183 k= 1,nbma1
             do 184 m=1,2
                if (numno1(m,k) == numn2t(i)) then
                   do 185 n=1,nbnomx
                      if (numno1(2+n,k) == numn1t(j)) then
                         do 186 p=1,6
                            do 187 q=1,3
                               m3dass(6*(i-1)+p,3*(j-1)+q)= m3dass(6*(i-1)+p,3*(j-1)+q) &
                                             + m3dea(6*(m-1)+p,3*(n-1)+q,k)
                            187 end do
                         186 end do
                      endif
                   185 end do
                endif
             184 end do
          183 end do
       182 end do
    181 end do

! --- CONCATENATION DES MATRICES DE COUPLAGE ASSEMBLEES

    do 171 i = 1,6*len2
        do 172 j = 1,6*len2
            mmixe1(i,j) = m1dass(i,j)
        172 end do
        do 173 j = 1,3*len1
            mmixe1(i,6*len2+j) = -1*m3dass(i,j)
        173 end do
    171 end do


! --- AFFECTATION DES RELATIONS ARLEQUIN

    nbterm=6*len2+3*len1

    do 91 i=1,nbterm
        coefc(i)=0
        jdime(i,:)=[0,0,0]
        direct(i,:)=[0,0,0]
        ddl1(i)= '000'
    91 end do

    do 92 i=1,len2
        do 96 j=1,6
            noeud1(6*(i-1)+j)='N'//numn2t(i)
        96 end do
        ddl1(6*i-5)='DX '
        ddl1(6*i-4)='DY '
        ddl1(6*i-3)='DZ '
        ddl1(6*i-2)='DRX'
        ddl1(6*i-1)='DRY'
        ddl1(6*i)  ='DRZ'
    92 end do

    do 93 i=1,len1
        do 97 j=1,3
            noeud1(6*len2+3*(i-1)+j)='N'//numn1t(i)
        97 end do
        ddl1(6*len2+3*i-2)='DX '
        ddl1(6*len2+3*i-1)='DY '
        ddl1(6*len2+3*i)  ='DZ '
    93 end do

    beta = 0.0d0
    betac = (1.0d0,0.0d0)
    typcoe = 'REEL'
    typval = 'REEL'
    typlag = '12'

    do 90 i = 1,6*len2
        coefri=mmixe1(i,:)
        call afrela(coefri(1:nbterm),coefc(1:nbterm),&
                    ddl1(1:nbterm),noeud1(1:nbterm),jdime(1:nbterm,:),direct(1:nbterm,:),&
                    nbterm,beta,betac,' ',typcoe,typval,typlag,0.d0,lisrel)
    90 end do

    call jedema()

end subroutine
