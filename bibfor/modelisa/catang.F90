subroutine catang(noma, nbma, listma, nbno, listno)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/normev.h"
#include "asterfort/wkvect.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: noma
    integer, intent(in) :: nbma
    integer, intent(in) :: listma(*)
    integer, intent(in) :: nbno
    integer, intent(in) :: listno(*)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Compute tangents at nodes on edge in 3D
!
! --------------------------------------------------------------------------------------------------
!
!
! In  noma   : mesh
! In  nbma   : number of elements
! In  listma : list of elements
! In  nbno   : number of nodes
! In  listno : list of nodes
!
! Objects created:
!     &&CATANG.TANGENT : tangents at nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, iacnx1, ilcnx1, jco, jtanma, j, ino, nbnoma, jcoor, jnoema
    integer :: jtanno, iret, k, ino1, ino2, ino3, jtyp, i1, jtang
    real(kind=8) :: vale1(3), vale2(3), vale3(3), vale(3), valu(3), valv(3)
    real(kind=8) :: norm
    character(len=8) :: ntyp, k8b
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoma,&
                k8b, iret)
    call jeveuo(noma//'.TYPMAIL', 'L', jtyp)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
    call jedetr('&&CATANG.TANGENT')
    call wkvect('&&CATANG.TANG_MAIL', 'V V R', 3*3*nbma, jtanma)
    call wkvect('&&CATANG.NOEU_MAIL', 'V V I', 3*nbma, jnoema)
    call wkvect('&&CATANG.TANG_NOEU', 'V V R', 3*nbnoma, jtanno)
    call wkvect('&&CATANG.TANGENT', 'V V R', 3*nbno, jtang)
!
! --- 1.ON CONSTRUIT LES VECTEURS TANGENTS AUX NOEUDS PAR ELEMENT
!     (LES ELEMENTS SONT CEUX SPECIFIES PAR L'UTILISATEUR)
!     ON LES STOCKE DANS TANMA=ZR(JTANMA)
!             <-- NOEUD 1--> <-- NOEUD 2-->  <-- NOEUD 3-->
!     TANMA=( V1X1,V1Y1,V1Z1,V1X2,V1Y2,V1Z2,V1X3,V1Y3,V1Z3, <= MAILLE 1
!             V2X1,V2Y1,V2Z1,V2X2,V2Y2,V2Z2,V2X3,V2Y3,V2Z3, <= MAILLE 2
!             ............................................
!             VNX1,VNY1,VNZ1,VNX2,VNY2,VNZ2,VNX3,VNY3,VNZ3) <= MAILLE N
!     REMARQUE : LE NOMBRE MAX DE NOEUDS PAR MAILLE EST 3.
!     DANS LE CAS SEG2, LES VALEURS TANGENTIELLES DU NOEUD 3 NE SONT PAS
!     PRISE EN COMPTE, ON INITIALISE AVEC LA VALEUR 0.D0
!
    do i = 1, nbma
!
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtyp+listma(i)-1)), ntyp)
        if (ntyp(1:3) .ne. 'SEG') ASSERT(.false.)
!
        jco=iacnx1-1+zi(ilcnx1+listma(i)-1)
        ino1=zi(jco)
        ino2=zi(jco+1)
        do j = 1, 3
            vale1(j)=zr(jcoor+3*(ino1-1)+j-1)
            vale2(j)=zr(jcoor+3*(ino2-1)+j-1)
        end do
!
        if (ntyp(1:4) .eq. 'SEG2') then
!
            do j = 1, 3
                vale(j)=vale2(j)-vale1(j)
            end do
            call normev(vale, norm)
!
!            -- PREMIER NOEUD DE LA MAILLE J
            zi(jnoema+3*(i-1))=ino1
            zr(jtanma+9*(i-1)) =vale(1)
            zr(jtanma+9*(i-1)+1)=vale(2)
            zr(jtanma+9*(i-1)+2)=vale(3)
!            -- SECOND NOEUD DE LA MAILLE J
            zi(jnoema+3*(i-1)+1)=ino2
            zr(jtanma+9*(i-1)+3)=vale(1)
            zr(jtanma+9*(i-1)+4)=vale(2)
            zr(jtanma+9*(i-1)+5)=vale(3)
!            -- NOEUD MILIEU DE LA MAILLE J
            zi(jnoema+3*(i-1)+2)=0
            zr(jtanma+9*(i-1)+6)=0.d0
            zr(jtanma+9*(i-1)+7)=0.d0
            zr(jtanma+9*(i-1)+8)=0.d0
!
        else if (ntyp(1:4).eq.'SEG3') then
!
            ino3=zi(jco+2)
            do j = 1, 3
                vale3(j)=zr(jcoor+3*(ino3-1)+j-1)
                valu(j) =vale3(j)-vale1(j)
                valv(j) =vale2(j)-vale3(j)
            end do
            call normev(valu, norm)
            call normev(valv, norm)
            do j = 1, 3
                vale(j)=valu(j)+valv(j)
            end do
            call normev(vale, norm)
!
!            -- PREMIER NOEUD DE LA MAILLE J
            zi(jnoema+3*(i-1))=ino1
            zr(jtanma+9*(i-1)) =valu(1)
            zr(jtanma+9*(i-1)+1)=valu(2)
            zr(jtanma+9*(i-1)+2)=valu(3)
!            -- SECOND NOEUD DE LA MAILLE J
            zi(jnoema+3*(i-1)+1)=ino2
            zr(jtanma+9*(i-1)+3)=valv(1)
            zr(jtanma+9*(i-1)+4)=valv(2)
            zr(jtanma+9*(i-1)+5)=valv(3)
!            -- NOEUD MILIEU DE LA MAILLE J
            zi(jnoema+3*(i-1)+2)=ino3
            zr(jtanma+9*(i-1)+6)=vale(1)
            zr(jtanma+9*(i-1)+7)=vale(2)
            zr(jtanma+9*(i-1)+8)=vale(3)
!
        endif
    end do
!
! --- 2.ON CONSTRUIT LES VECTEURS TANGENTS AUX NOEUDS
!     ON LES STOCKE DANS TNNO= ZR(JTANNO) DIMENSIONNE
!     AU NOMBRE DE NOEUDS DU MAILLAGE.
!     TANNO=( V1X,V1Y,V1Z, <= NOEUD 1
!             V2X,V2Y,V2Z, <= NOEUD 2
!             ............,
!             VNX,VNY,VNZ )<= NOEUD N
!     REMARQUE : ViX=ViY=ViZ=0 SI LE NOEUD i NE FAIT PAS
!     PARTIE DES MAILLES FOURNIES PAR L'UTILISATEUR
!
    do j = 1, 3*nbnoma
        zr(jtanno+j-1)=0.d0
    end do
    do j = 1, nbma
        do i = 1, 3
            ino=zi(jnoema+3*(j-1)+i-1)
            if (ino .gt. 0) then
                do k = 1, 3
                    zr(jtanno+3*(ino-1)+k-1)=zr(jtanno+3*(ino-1)+k-1)+&
                    zr(jtanma+9*(j-1)+3*(i-1)+k-1)
                end do
            endif
        end do
    end do
!
! --- 3.ON FILTRE POUR NE CONSERVER QUE LES VECTEURS TANGENTS AUX NOEUDS
!     DEMANDES PAR L'UTILISATEUR (PRISE EN COMPTE DE SANS_XXXX)
!
    do j = 1, 3*nbno
        zr(jtang-1+j)=0.d0
    end do
    do j = 1, nbma
        do i = 1, 3
            ino=zi(jnoema+3*(j-1)+i-1)
            i1= indiis(listno,ino,1,nbno)
            if (i1 .gt. 0) then
                vale(1)=zr(jtanno+3*(ino-1))
                vale(2)=zr(jtanno+3*(ino-1)+1)
                vale(3)=zr(jtanno+3*(ino-1)+2)
                call normev(vale, norm)
                do k = 1, 3
                    zr(jtang-1+3*(i1-1)+k) = vale(k)
                end do
            endif
        end do
    end do
!
    call jedetr('&&CATANG.TANG_MAIL')
    call jedetr('&&CATANG.NOEU_MAIL')
    call jedetr('&&CATANG.TANG_NOEU')
!
    call jedema()
end subroutine
