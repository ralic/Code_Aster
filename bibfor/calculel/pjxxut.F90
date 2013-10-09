subroutine pjxxut(dim, mocle, moa1, moa2, nbma1,&
                  lima1, nbno2, lino2, ma1, ma2,&
                  nbtmx, nbtm, nutm, elrf)
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/pjnout.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=2) :: dim
    character(len=8) :: moa1, moa2, ma1, ma2
    character(len=*) :: mocle
    integer :: nbma1, lima1(*), nbno2, lino2(*), nbtmx, nbtm, nutm(nbtmx)
    character(len=8) :: elrf(nbtmx)
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! BUT :
!   PREPARER LA LISTE DES MAILLES ET LES LISTES DE NOEUDS
!   UTILES A LA PROJECTION:
!
!   CETTE ROUTINE PRODUIT LES OBJETS SUIVANTS :
!    '&&PJXXCO.LIMA1' : NUMEROS DES MAILLES UTILES DE MOA1
!    '&&PJXXCO.LINO1' : NUMEROS DES NOEUDS UTILES DE MOA1
!    '&&PJXXCO.LINO2' : NUMEROS DES NOEUDS UTILES DE MOA2
!
!   M1 EST LE NOM DU MAILLAGE (OU DU MODELE) INITIAL
!   M2 EST LE NOM DU MAILLAGE (OU DU MODELE) FINAL
!
!   LES MAILLES UTILES DE MOA1 SONT CELLES QUI :
!      - SONT D'UN TYPE COHERENT AVEC DIM :
!             PAR EXEMPLE : '2D' -> TRIA/QUAD
!      - SONT PORTEUSES D'ELEMENTS FINIS (SI M1 EST UN MODELE)
!      - SONT INCLUSES DANS LIMA1 (SI MOCLE='PARTIE')
!
!   LES NOEUDS UTILES DE MOA1 SONT CEUX QUI SONT PORTES PAR LES
!   MAILLES UTILES DE MOA1
!
!   LES NOEUD UTILES DE MOA2 SONT CEUX QUI :
!      - SONT PORTES PAR LES MAILLES SUPPORTANT LES ELEMENTS FINIS
!        (SI M1 EST UN MODELE)
!      - SONT INCLUS DANS LINO2 (SI MOCLE='PARTIE')
!
!  SI MOCLE='TOUT' :
!     - ON NE SE SERT PAS DE NBMA1,LIMA1,NBNO2,LINO2
!
!-----------------------------------------------------------------------
!  IN        DIM    K2:  /'1D'  /'2D'  /'3D'
!  IN        MOCLE  K*:  /'TOUT'  /'PARTIE'
!
!  IN/JXIN   MOA1    K8  : NOM DU MAILLAGE (OU MODELE) INITIAL
!  IN/JXIN   MOA2    K8  : NOM DU MAILLAGE (OU MODELE) SUR LEQUEL ON
!                          VEUT PROJETER
!
!  IN        NBMA1    I   : NOMBRE DE MAILLES DE LIMA1
!  IN        LIMA1(*) I   : LISTE DE NUMEROS DE MAILLES (DE MOA1)
!  IN        NBNO2    I   : NOMBRE DE NOEUDS DE LINO2
!  IN        LINO2(*) I   : LISTE DE NUMEROS DE NOEUDS (DE MOA2)
!  OUT       MA1      K8  : NOM DU MAILLAGE ASSOCIE A MOA1
!  OUT       MA2      K8  : NOM DU MAILLAGE ASSOCIE A MOA2
!  IN        NBTMX    I   : DIMENSION DU TABLEAU NUTM
!  OUT       NBTM     I   : NOMBRE DE TYPE_MAILLE POUR DIM
!  OUT       NUTM(*)  I   : NUMEROS DES TYPE_MAILLE POUR DIM
!  OUT       ELRF(*)  K8  : ELREFES DES TYPE_MAILLE POUR DIM
! ----------------------------------------------------------------------
!
!
    character(len=8) ::  mo1, mo2
    character(len=8) :: notm(nbtmx)
!
    integer ::  nno1, nno2, nma1, nma2, i, k, j
    integer :: ima, nbno, ino, nuno, ino2, kk, ima1
    integer :: ialim1, iad, long, ialin1, iacnx1, ilcnx1, ialin2
    integer :: iexi
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
!     MOA1 EST IL UN MODELE OU UN MAILLAGE ?
    call jeexin(moa1//'.MODELE    .NBNO', iexi)
    if (iexi .gt. 0) then
        mo1=moa1
        call dismoi('NOM_MAILLA', mo1, 'MODELE', repk=ma1)
    else
        mo1=' '
        ma1=moa1
    endif
!
!     MOA2 EST IL UN MODELE OU UN MAILLAGE ?
    call jeexin(moa2//'.MODELE    .NBNO', iexi)
    if (iexi .gt. 0) then
        mo2=moa2
        call dismoi('NOM_MAILLA', mo2, 'MODELE', repk=ma2)
        call pjnout(mo2)
    else
        mo2=' '
        ma2=moa2
    endif
!
!
    call dismoi('NB_NO_MAILLA', ma1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', ma2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', ma1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', ma2, 'MAILLAGE', repi=nma2)
!
!
!
!     1 : TYPE_MAILLES UTILES DE MOA1 :
!     ----------------------------------
    if (dim .eq. '1D') then
        nbtm=3
        notm(1)='SEG2'
        notm(2)='SEG3'
        notm(3)='SEG4'
!
        elrf(1)='SE2'
        elrf(2)='SE3'
        elrf(3)='SE4'
    else if (dim.eq.'2D') then
        nbtm=6
        notm(1)='TRIA3'
        notm(2)='TRIA6'
        notm(3)='TRIA7'
        notm(4)='QUAD4'
        notm(5)='QUAD8'
        notm(6)='QUAD9'
!
        elrf(1)='TR3'
        elrf(2)='TR6'
        elrf(3)='TR7'
        elrf(4)='QU4'
        elrf(5)='QU8'
        elrf(6)='QU9'
    else if (dim.eq.'3D') then
        nbtm=10
        notm(1)='TETRA4'
        notm(2)='TETRA10'
        notm(3)='PENTA6'
        notm(4)='PENTA15'
        notm(5)='PENTA18'
        notm(6)='HEXA8'
        notm(7)='HEXA20'
        notm(8)='HEXA27'
        notm(9)='PYRAM5'
        notm(10)='PYRAM13'
!
        elrf(1)='TE4'
        elrf(2)='T10'
        elrf(3)='PE6'
        elrf(4)='P15'
        elrf(5)='P18'
        elrf(6)='HE8'
        elrf(7)='H20'
        elrf(8)='H27'
        elrf(9)='PY5'
        elrf(10)='P13'
    else
        ASSERT(.false.)
    endif
!
    do 10 k = 1, nbtm
        call jenonu(jexnom('&CATA.TM.NOMTM', notm(k)), nutm(k))
 10 end do
!
!
!
!     2 : MAILLES UTILES DE MOA1 :
!     ----------------------------
    call wkvect('&&PJXXCO.LIMA1', 'V V I', nma1, ialim1)
    if (mo1 .ne. ' ') then
        call jeveuo(mo1//'.MAILLE', 'L', iad)
        call jelira(mo1//'.MAILLE', 'LONMAX', long)
        do 20 i = 1, long
            if (zi(iad-1+i) .ne. 0) zi(ialim1-1+i)=1
 20     continue
    else
        do 30 i = 1, nma1
            zi(ialim1-1+i)=1
 30     continue
    endif
!
    call jeveuo(ma1//'.TYPMAIL', 'L', iad)
    do 50 j = 1, nbtm
        do 40 i = 1, nma1
            if (zi(iad-1+i) .eq. nutm(j)) zi(ialim1-1+i)=zi(ialim1-1+i)+ 1
 40     continue
 50 end do
!
    do 60 i = 1, nma1
        if (zi(ialim1-1+i) .eq. 1) then
            zi(ialim1-1+i)=0
        else if (zi(ialim1-1+i).eq.2) then
            zi(ialim1-1+i)=1
        else if (zi(ialim1-1+i).gt.2) then
            ASSERT(.false.)
        endif
 60 end do
!
    if (mocle .eq. 'PARTIE') then
        do 70 ima1 = 1, nbma1
            zi(ialim1-1+lima1(ima1))=2*zi(ialim1-1+lima1(ima1))
 70     continue
        do 80 ima1 = 1, nma1
            zi(ialim1-1+ima1)=zi(ialim1-1+ima1)/2
 80     continue
    endif
!
!
!     3 : NOEUDS UTILES DE MOA1 :
!     ---------------------------
    call wkvect('&&PJXXCO.LINO1', 'V V I', nno1, ialin1)
    call jeveuo(ma1//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(ma1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    do 100 ima = 1, nma1
        if (zi(ialim1-1+ima) .eq. 0) goto 100
        nbno=zi(ilcnx1+ima)-zi(ilcnx1-1+ima)
        do 90 ino = 1, nbno
            nuno=zi(iacnx1+zi(ilcnx1-1+ima)-2+ino)
            zi(ialin1-1+nuno)=1
 90     continue
100 end do
!
!
!     4 : NOEUDS UTILES DE MOA2 :
!     ---------------------------
    call wkvect('&&PJXXCO.LINO2', 'V V I', nno2, ialin2)
!
    if (mo2 .ne. ' ') then
        call jeveuo(mo2//'.NOEUD_UTIL', 'L', iad)
        if (mocle .eq. 'TOUT') then
            do 110 ino = 1, nno2
                if (zi(iad-1+ino) .ne. 0) zi(ialin2-1+ino)=1
110         continue
        else if (mocle.eq.'PARTIE') then
            do 120 ino2 = 1, nbno2
                if (zi(iad-1+lino2(ino2)) .ne. 0) zi(ialin2-1+lino2(ino2) )=1
120         continue
        endif
    else
        if (mocle .eq. 'TOUT') then
            do 130 ino = 1, nno2
                zi(ialin2-1+ino)=1
130         continue
        else if (mocle.eq.'PARTIE') then
            do 140 ino2 = 1, nbno2
                zi(ialin2-1+lino2(ino2))=1
140         continue
        endif
    endif
!
!
!     ON ARRETE S'IL N'Y A PAS DE NOEUDS "2" :
!     ------------------------------------------------
    kk=0
    do 150 k = 1, nno2
        if (zi(ialin2-1+k) .gt. 0) kk=kk+1
150 end do
    if (kk .eq. 0) then
        call utmess('F', 'CALCULEL4_54')
    endif
!
    call jedema()
end subroutine
