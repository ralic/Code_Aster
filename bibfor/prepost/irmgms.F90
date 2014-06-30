subroutine irmgms(ifc, ndim, nno, noma, nbgrm,&
                  nonoe, lgmsh, versio)
    implicit none
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/infniv.h"
#include "asterfort/irgmm3.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxliis.h"
#include "asterfort/nutygm.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: noma, nonoe(*)
    integer :: ifc, versio
    logical(kind=1) :: lgmsh
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
!     BUT :   ECRITURE DU MAILLAGE AU FORMAT GMSH
!     ENTREE:
!       IFC    : NUMERO D'UNITE LOGIQUE DU FICHIER GMSH
!       NDIM   : DIMENSION DU PROBLEME (2  OU 3)
!       NNO    : NOMBRE DE NOEUDS DU MAILLAGE
!       NOMA   : NOM DU MAILLAGE
!       NBGRM  : NOMBRE DE GROUPES DE MAILLES
!       NONOE  : NOM DES NOEUDS
!       LGMSH  : VRAI SI MAILLAGE PRODUIT PAR PRE_GMSH (DANS CE CAS,
!                ON CONSERVE LES NUMEROS DE NOEUDS ET DE MAILLES)
!       VERSIO :  =1 SI ON NE PREND QUE DES MAILLES TRI3 ET TET4
!                 =2 SI ON PREND TOUTES LES MAILLES LINEAIRES
!
!     ------------------------------------------------------------------
! ---------------------------------------------------------------------
!
    real(kind=8) :: zero
    character(len=8) :: nomaou, nomtyp
    character(len=8) :: k8nno, k8nbma
    character(len=24) :: typmai, nommai, nomail, valk, nomgrm
    integer :: typpoi, typseg, typtri, typtet, typqua, typpyr, typpri, typhex
!
!     --- TABLEAU DE DECOUPAGE
    integer :: ntyele
!-----------------------------------------------------------------------
    integer :: i, iatyma, ibid, idgm, idgrma, idlima, idm
    integer :: idn,  ier, igm, ima, ino, ipoin
    integer :: itype, itypgm, j,    jpoin
    integer :: nbelgm, nbgrm, nbm, nbma2, nbmli, ndim, nno
    integer :: nnoe, numgrm, numgrx
!-----------------------------------------------------------------------
    parameter (ntyele = 28)
!     NBRE, NOM D'OBJET POUR CHAQUE TYPE D'ELEMENT
    integer :: nbel(ntyele)
    character(len=24) :: nobj(ntyele)
    character(len=7) :: k7no, k7ma, tk7no(ntyele)
    character(len=8) :: nmtyp(ntyele), blanc8
    integer :: nbtyp(ntyele), ifm, niv, vali
    integer, pointer :: numgrma(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: nbnune(:) => null()
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
!
!     FORMAT DES NOMS DE NOEUDS (N1234567 : IDN=2, ROUTINE GMENEU)
!                    DE MAILLES (M1234567 : IDM=2, ROUTINE GMEELT)
!          ET GROUPE DE MAILLES (GM123456 : IDGM=3, ROUTINE GMEELT)
!     PRODUITS PAR PRE_GMSH
    idn=2
    idm=2
    idgm=3
    blanc8='        '
!
! --- TRANSFORMATION DES MAILLES D'ORDRE 2 DU MAILLAGE EN MAILLES
! --- D'ORDRE 1 :
!     =========
    zero = 0.0d0
    nomaou = '&&MAILLA'
    ibid = 1
!
    typmai = nomaou//'.TYPMAIL'
    nommai = nomaou//'.NOMMAI'
!
! --- INIT
    do 101 i = 1, ntyele
        nbel(i) = 0
        nobj(i) = ' '
101  end do
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1' ), typpoi)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2' ), typseg)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3' ), typtri)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4' ), typqua)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA4' ), typtet)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM5' ), typpyr)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA6' ), typpri)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA8' ), typhex)
    nobj(typpoi) = nomaou//'_POI'
    nobj(typseg) = nomaou//'_SEG'
    nobj(typtri) = nomaou//'_TRI'
    nobj(typqua) = nomaou//'_QUA'
    nobj(typtet) = nomaou//'_TET'
    nobj(typpyr) = nomaou//'_PYR'
    nobj(typpri) = nomaou//'_PRI'
    nobj(typhex) = nomaou//'_HEX'
!
    call irgmm3(noma, nomaou, 0, [ibid], 'V',&
                nobj, nbel, versio)
!
    call jeveuo(nomaou//'.COORDO    .VALE', 'L', vr=vale)
    call jeveuo(nomaou//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(nomaou//'.CONNEX', 'LONCUM'), 'L', jpoin)
    call jeveuo(typmai, 'L', iatyma)
    call jeveuo(nomaou//'.NBNUNE', 'L', vi=nbnune)
!
! --- ECRITURE DES NOEUDS DU MAILLAGE SUR LE FICHIER GMSH :
!     ===================================================
    write(ifc,'(A4)') '$NOD'
    call codent(nno, 'G', k8nno)
    write(ifc,'(A8)') k8nno
!
!
    do 10 ino = 1, nno
        if (lgmsh) then
            k7no=nonoe(ino)(idn:8)
        else
            call codent(ino, 'D', k7no)
        endif
        if (ndim .eq. 3) then
            write(ifc,1001) k7no, (vale(1+3*(ino-1)+j-1),j=1,ndim)
        else if (ndim.eq.2) then
            write(ifc,1001) k7no, (vale(1+3*(ino-1)+j-1),j=1,ndim),&
            zero
        endif
10  end do
!
    write(ifc,'(A7)') '$ENDNOD'
!
    nbma2 = 0
    do 102 i = 1, ntyele
        nbma2 = nbma2 + nbel(i)
102  end do
!
! --- NUMERO DES GROUP_MA (POUR L'ECRITURE DES MAILLES) :
!     =================================================
! --- CREATION DU VECTEUR DES NUMEROS DE GROUP_MA :
!     -------------------------------------------
    AS_ALLOCATE(vi=numgrma, size=nbma2)
!
!     LES NOMS DE GROUPE DE MAILLES SONT AU FORMAT : GM123456
!
    numgrx = 10000
    do 20 igm = 1, nbgrm
        call jenuno(jexnum(noma//'.GROUPEMA', igm), nomgrm)
        call lxliis(nomgrm(idgm:24), numgrm, ier)
        if (ier .eq. 0) then
            numgrx = max(numgrx,numgrm)
        endif
20  end do
!
    if (nbgrm .gt. 0) then
        call utmess('I', 'PREPOST6_31')
    endif
!
    do 30 igm = 1, nbgrm
        call jenuno(jexnum(noma//'.GROUPEMA', igm), nomgrm)
        call jeveuo(jexnom(noma//'.GROUPEMA', nomgrm), 'L', idgrma)
        call lxliis(nomgrm(idgm:24), numgrm, ier)
        if (ier .eq. 1) then
            numgrm = igm + numgrx
        endif
        call jelira(jexnum(noma//'.GROUPEMA', igm), 'LONUTI', nbm)
        do 40 i = 1, nbm
            nbmli = nbnune(1+zi(idgrma+i-1)-1)
            call jeveuo(jexnum( '&&IRMGMS.LISMA', zi(idgrma+i-1) ), 'L', idlima)
            do 50 j = 1, nbmli
                numgrma(1+zi(idlima+j-1)-1) = numgrm
50          continue
40      continue
        write(6,1002) nomgrm,numgrm
30  end do
!
! --- ECRITURE DES MAILLES DU MAILLAGE SUR LE FICHIER GMSH :
!     ====================================================
    write(ifc,'(A4)') '$ELM'
    call codent(nbma2, 'G', k8nbma)
    write(ifc,'(A8)') k8nbma
!
    do 70 i = 1, ntyele
        nmtyp(i) = blanc8
        nbtyp(i) = 0
70  end do
    nbelgm =0
    do 60 ima = 1, nbma2
        ipoin = zi(jpoin+ima-1)
        nnoe = zi(jpoin+ima) - ipoin
!
! ---    NOM DU TYPE DE L'ELEMENT :
!        ------------------------
        itype = zi(iatyma+ima-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', itype), nomtyp)
        itypgm = nutygm(nomtyp)
        if (numgrma(ima) .eq. 0) then
            nbelgm=nbelgm+1
            numgrma(ima) = 10000
!
            do 80 i = 1, ntyele
                if (nmtyp(i) .eq. nomtyp) then
                    nbtyp(i)=nbtyp(i)+1
                    goto 90
                else if (nmtyp(i).eq.blanc8) then
                    nbtyp(i)=nbtyp(i)+1
                    nmtyp(i)=nomtyp
                    goto 90
                endif
80          continue
90          continue
        endif
        call jenuno(jexnum(nommai, ima), nomail)
        if (lgmsh) then
            k7ma=nomail(idm:8)
        else
            call codent(ima, 'D', k7ma)
        endif
        do 61 ino = 1, nnoe
            if (lgmsh) then
                tk7no(ino)=nonoe(connex(1+ipoin-1+ino-1))(idn:8)
            else
                call codent(connex(1+ipoin-1+ino-1), 'D', tk7no(ino))
            endif
61      continue
        write(ifc,1003) k7ma,itypgm,numgrma(ima),numgrma(ima),&
        nnoe, (tk7no(ino),ino=1,nnoe)
60  end do
!
    if (nbtyp(1) .ne. 0 .and. niv .ge. 1) then
        call utmess('I', 'PREPOST6_32', si=nbelgm)
        do 95 i = 1, ntyele
            if (nmtyp(i) .ne. blanc8) then
                valk = nmtyp(i)
                vali = nbtyp(i)
                call utmess('I', 'PREPOST6_33', sk=valk, si=vali)
            endif
95      continue
    endif
!
    write(ifc,'(A7)') '$ENDELM'
!
! --- MENAGE
    call detrsd('MAILLAGE', nomaou)
    call jedetr(nomaou//'_POI')
    call jedetr(nomaou//'_SEG')
    call jedetr(nomaou//'_TRI')
    call jedetr(nomaou//'_QUA')
    call jedetr(nomaou//'_TET')
    call jedetr(nomaou//'_PYR')
    call jedetr(nomaou//'_PRI')
    call jedetr(nomaou//'_HEX')
    call jedetr(nomaou//'.NBNUNE')
    AS_DEALLOCATE(vi=numgrma)
    call jedetr('&&IRMGMS.LISMA')
!
    1001 format (1x,a7,1x,1pe23.16,1x,1pe23.16,1x,1pe23.16,1x,1pe23.16)
    1002 format (11x,a24,9x,i8)
    1003 format (1x,a7,1x,i2,1x,i8,1x,i8,1x,i8,27(1x,a7))
!
    call jedema()
end subroutine
