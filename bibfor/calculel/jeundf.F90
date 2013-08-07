subroutine jeundf(obj)
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
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
#include "asterc/ismaem.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: obj
! ----------------------------------------------------------------------
!     BUT : METTRE A "UNDEF" UN OBJET JEVEUX
!             I   :  ISMAEM()
!             L   :  .FALSE.
!             R   :  R8NNEM()
!             C   :  DCMPLX(R8NNEM(),R8NNEM())
!             K*  : 'XXXXXXXXXXXXXX'
!
!     OBJ   IN/JXVAR  K24 : NOM DE L'OBJET
!
    character(len=24) :: obj2
    real(kind=8) :: r1undf
    character(len=8) :: typsca, k8df
    character(len=16) :: k16df
    character(len=24) :: k24df
    character(len=32) :: k32df
    character(len=80) :: k80df
    character(len=1) :: xous, type
    complex(kind=8) :: c1undf
    integer :: long, i1undf, ltyp, iad, k
! DEB-------------------------------------------------------------------
!
    call jemarq()
    obj2=obj
!
    i1undf=ismaem()
    r1undf=r8nnem()
    c1undf=dcmplx(r1undf,r1undf)
    k8df='XXXXXXXX'
    k16df=k8df//k8df
    k24df=k16df//k8df
    k32df=k24df//k8df
    k80df=k32df//k32df//k16df
!
!
!     -- DETERMINATION DE TYPSCA :
!     -----------------------------
    call jelira(obj2, 'TYPE', cval=type)
    if (type .eq. 'K') then
        call jelira(obj2, 'LTYP', ltyp)
        if (ltyp .eq. 8) then
            typsca='K8'
        else if (ltyp.eq.16) then
            typsca='K16'
        else if (ltyp.eq.24) then
            typsca='K24'
        else if (ltyp.eq.32) then
            typsca='K32'
        else if (ltyp.eq.80) then
            typsca='K80'
        else
            ASSERT(.false.)
        endif
    else
        typsca=type
    endif
!
    call jelira(obj2, 'XOUS', cval=xous)
!     TEST CAS NON PROGRAMME
    ASSERT(xous.ne.'X')
!
    call jelira(obj2, 'LONMAX', long)
    call jeveuo(obj2, 'E', iad)
!
!
    if (typsca .eq. 'I') then
        do 10,k=1,long
        zi(iad-1+k)=i1undf
10      continue
    else if (typsca.eq.'L') then
        do 20,k=1,long
        zl(iad-1+k)=.false.
20      continue
    else if (typsca.eq.'R') then
        do 30,k=1,long
        zr(iad-1+k)=r1undf
30      continue
    else if (typsca.eq.'C') then
        do 40,k=1,long
        zc(iad-1+k)=c1undf
40      continue
    else if (typsca.eq.'K8') then
        do 50,k=1,long
        zk8(iad-1+k)=k8df
50      continue
    else if (typsca.eq.'K16') then
        do 60,k=1,long
        zk16(iad-1+k)=k16df
60      continue
    else if (typsca.eq.'K24') then
        do 70,k=1,long
        zk24(iad-1+k)=k24df
70      continue
    else if (typsca.eq.'K32') then
        do 80,k=1,long
        zk32(iad-1+k)=k32df
80      continue
    else if (typsca.eq.'K80') then
        do 90,k=1,long
        zk80(iad-1+k)=k80df
90      continue
    else
        ASSERT(.false.)
    endif
!
!
    call jedema()
end subroutine
