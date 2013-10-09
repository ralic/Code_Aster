subroutine pj5dco(mo1, mo2, corres)
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/dismoi.h"
#include "asterfort/exmano.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pacoa2.h"
#include "asterfort/pj3da4.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: corres
    character(len=8) :: mo1, mo2
! ----------------------------------------------------------------------
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
! ======================================================================
!     BUT :
!       CREER UNE SD CORRESP_2_MAILLA
!       DONNANT LA CORRESPONDANCE ENTRE LES NOEUDS DU MAILLAGE M1
!       ET CEUX DE M2 (DANS LE CAS DE MAILLAGE EN SEG2)
!
!  IN/JXIN   MO1      K8  : NOM DU MODELE INITIAL
!  IN/JXIN   MO2      K8  : NOM DU MODELE SUR LEQUEL ON VEUT PROJETER
!                           DES CHAMPS
!  IN/JXOUT  CORRES  K16 : NOM DE LA SD CORRESP_2_MAILLA
!
! ----------------------------------------------------------------------
!
!
!
    integer :: nbmail, nbdim, nno1, nno2, nbno
    integer :: llin1, llin2, inode, nbtr, lno1, lno2, lco1, lco2, idecal
    integer :: out1, out2, jcoo1, jcoo2, iacnx1, ilcnx1, numnoe
    integer :: i, nbmano, ima, imail, ino, j2xxk1, i2conb, i2conu, i2cocf
    parameter (nbmail=10)
    parameter (nbdim=3)
    integer :: numano(nbmail), nunoe(nbmail)
    real(kind=8) :: a(nbdim), b(nbdim), m(nbdim), un, deux
    real(kind=8) :: dpmin, l1, l2, xabs, dp, am(nbdim), bm(nbdim), a1, a2, dist
    character(len=8) :: m1, m2
    character(len=16) :: lisin1, lisin2, lisou1, lisou2
    character(len=16) :: noeud1, noeud2, cobar1, cobar2
    character(len=24) :: coormo, coorme
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    un = 1.d0
    deux = 2.d0
!
    call dismoi('NOM_MAILLA', mo1, 'MODELE', repk=m1)
    call dismoi('NOM_MAILLA', mo2, 'MODELE', repk=m2)
!
    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
!
    if (nno2 .eq. 0) then
        call utmess('F', 'CALCULEL4_54')
    endif
!
!     DETERMINATION DE LA DIMENSION DE L'ESPACE :
!     --------------------------------------------------------
!
!     Initialisation des A, B et M
    do i = 1, nbdim
        a(i) = 0.d0
        b(i) = 0.d0
        m(i) = 0.d0
        am(i) = 0.d0
        bm(i) = 0.d0
    end do
!
    coormo = m1//'.COORDO    .VALE'
    call jeveuo(coormo, 'L', jcoo1)
!
    coorme = m2//'.COORDO    .VALE'
    call jeveuo(coorme, 'L', jcoo2)
!
!     1. RECHECHE DES MAILLES LIEES AU NOEUD LE PLUS PROCHE
!     ------------------------------------------------
    lisin1 = 'NOEUD_MODELE'
    call wkvect(lisin1, 'V V K8', nno1, llin1)
!
    lisin2 = 'NOEUD_MESURE'
    call wkvect(lisin2, 'V V K8', nno2, llin2)
!
    lisou1 = 'NOEUD_MODELE_VIS'
    lisou2 = 'NOEUD_MESURE_VIS'
!
    do inode = 1, nno1
        call jenuno(jexnum (m1//'.NOMNOE', inode), zk8(llin1-1+inode))
    end do
    do inode = 1, nno2
        call jenuno(jexnum (m2//'.NOMNOE', inode), zk8(llin2-1+inode))
    end do
!
    call pacoa2(lisin1, lisin2, nno1, nno2, m1,&
                m2, lisou1, lisou2, nbtr)
    if (nbtr .ne. nno2) then
        call utmess('F', 'ALGORITH9_91')
    endif
!
    noeud1 = 'NOEUD_DEBUT'
    call wkvect(noeud1, 'V V I', nbtr, lno1)
!
    noeud2 = 'NOEUD_FIN'
    call wkvect(noeud2, 'V V I', nbtr, lno2)
!
    cobar1 = 'COEFF_DEBUT'
    call wkvect(cobar1, 'V V R', nbtr, lco1)
!
    cobar2 = 'COEFF_FIN'
    call wkvect(cobar2, 'V V R', nbtr, lco2)
!
    call jeveuo(lisou1, 'L', out1)
    call jeveuo(lisou2, 'L', out2)
!
    call jeveuo(m1//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
!     2. RECHECHE DE LA MAILLE LE PLUS PROCHE DU NOEUD MESURE
!     ------------------------------------------------
    do inode = 1, nbtr
        call jenonu(jexnom(m2//'.NOMNOE', zk8(out2-1+inode)), numnoe)
        do i = 1, nbdim
            m(i) = zr(jcoo2-1 +(numnoe-1)*nbdim+i)
        end do
!
        call jenonu(jexnom(m1//'.NOMNOE', zk8(out1-1+inode)), numnoe)
        call exmano(m1, numnoe, numano, nbmano)
        if (nbmano .eq. 0) then
            call utmess('F', 'ALGORITH9_92')
        endif
        dpmin = r8maem()
        do ima = 1, nbmano
            imail = numano(ima)
            nbno=zi(ilcnx1-1 +imail+1)-zi(ilcnx1-1 +imail)
!     THEORIQUEMENT NBNO = 2 (POUR SEG2)
            do ino = 1, nbno
                nunoe(ino)=zi(iacnx1-1 +zi(ilcnx1-1 +imail)-1+ino)
            end do
            do i = 1, nbdim
                a(i) = zr(jcoo1-1 +(nunoe(1)-1)*nbdim+i)
                b(i) = zr(jcoo1-1 +(nunoe(2)-1)*nbdim+i)
            end do
!
!     3. CALCUL DE LA DISTANCE NOEUD-MAILLE (AM + BM)
!     ------------------------------------------------
            do i = 1, nbdim
                am(i)=m(i)-a(i)
                bm(i)=m(i)-b(i)
            end do
!
            a1 = 0.d0
            a2 = 0.d0
            do i = 1, nbdim
                a1= a1 + am(i)*am(i)
                a2= a2 + bm(i)*bm(i)
            end do
            dist = sqrt(a1)+sqrt(a2)
!
            if (dist .lt. dpmin) then
                dpmin = dist
!
!     4. CALCUL DES COORDONNEES BARYCENTRIQUES
!     ------------------------------------------------
                call pj3da4(m, a, b, l1, l2,&
                            dp)
                zi(lno1-1 +inode) = nunoe(1)
                zi(lno2-1 +inode) = nunoe(2)
                zr(lco1-1 +inode) = l1
                zr(lco2-1 +inode) = l2
            endif
        end do
!
!     5. APPLICATION FONCTION DE FORME (ELEMENT ISOPARAMETRIQUE)
!     ---------------------------------------------------
!     XABS : ABSCISSE DU POINT DANS L'ELEMENT DE REFERENCE (SEG2)
        xabs = -un*zr(lco1-1 +inode) + un*zr(lco2-1 +inode)
        zr(lco1-1 +inode) = (un-xabs)/deux
        zr(lco2-1 +inode) = (un+xabs)/deux
    end do
!
!     6. CREATION DE LA SD CORRESP_2_MAILLA : CORRES
!     ---------------------------------------------------
    call wkvect(corres//'.PJXX_K1', 'V V K24', 5, j2xxk1)
    call wkvect(corres//'.PJEF_NB', 'V V I', nno2, i2conb)
    call wkvect(corres//'.PJEF_NU', 'V V I', nno2*2, i2conu)
    call wkvect(corres//'.PJEF_CF', 'V V R', nno2*2, i2cocf)
!
    zk24(j2xxk1-1 +1)=m1
    zk24(j2xxk1-1 +2)=m2
    zk24(j2xxk1-1 +3)='COLLOCATION'
!
    do ino = 1, nno2
        zi(i2conb-1 +ino)=2
    end do
!
    idecal=0
    do ino = 1, nno2
        zi(i2conu-1 +idecal+1)=zi(lno1-1 +ino)
        zi(i2conu-1 +idecal+2)=zi(lno2-1 +ino)
        zr(i2cocf-1 +idecal+1)=zr(lco1-1 +ino)
        zr(i2cocf-1 +idecal+2)=zr(lco2-1 +ino)
        idecal = idecal+zi(i2conb-1 +ino)
    end do
!
!
    call jedetr(lisin1)
    call jedetr(lisin2)
    call jedetr(noeud1)
    call jedetr(noeud2)
    call jedetr(cobar1)
    call jedetr(cobar2)
!
    call jedema()
end subroutine
