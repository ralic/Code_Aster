subroutine momaba(mailla)
    implicit none
#include "jeveux.h"
#include "asterfort/barhex.h"
#include "asterfort/barpen.h"
#include "asterfort/barpyr.h"
#include "asterfort/barqua.h"
#include "asterfort/bartet.h"
#include "asterfort/bartri.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mailla
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
    integer :: jtyma, nbmc, nbma, jnuma, i, j, ityp, jcoor, n1, n2, i1, i2, nbno
    integer :: nbmat, jpoin, ier, ifm, niv, jcon, jconm, ndim, nn, jnbma, ncount
    integer :: jdim
    logical :: lnmf, lmodi
    parameter(nbmc=2)
    character(len=8) :: k8b, type
    character(len=16) :: tymocl(nbmc), motcle(nbmc)
    character(len=24) :: connex, nommai, nomnoe, nomjv
!     ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    connex=mailla//'.CONNEX'
    nommai=mailla//'.NOMMAI'
    nomnoe=mailla//'.NOMNOE'
    call jeveuo(mailla//'.TYPMAIL        ', 'L', jtyma)
    call jeveuo(mailla//'.COORDO    .VALE', 'E', jcoor)
    call jeveuo(mailla//'.DIME           ', 'L', jdim)
    call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbmat,&
                k8b, ier)
    lmodi=.false.
!     ------------------------------------------------------------------
!
! --- LECTURE DE LA LISTE DE MAILLES
!
    motcle(1)='GROUP_MA_FOND'
    tymocl(1)='GROUP_MA'
    motcle(2)='MAILLE_FOND'
    tymocl(2)='MAILLE'
    nomjv='&&MOMABA.LISTE_MA'
    call reliem(' ', mailla, 'NU_MAILLE', 'MODI_MAILLE', 1,&
                nbmc, motcle, tymocl, nomjv, nbma)
!
    if (nbma .eq. 0) goto 150
!
    call jeveuo(nomjv, 'L', jnuma)
!
! --- TRAITEMENT DES MAILLES
!
!     ON INTERDIT 'GROUP_MA_FOND' ET 'MAILLE_FOND'
!     SI LE MAILLAGE EST DE DIMENSION 2.
    if (zi(jdim+5) .eq. 2) then
        call utmess('F', 'ALGORITH6_16')
    endif
!
    do 140 i = 1, nbma
        ityp=jtyma-1+zi(jnuma+i-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
!
        if (niv .eq. 2) then
            call jenuno(jexnum(nommai, zi(jnuma+i-1)), k8b)
            write (ifm,*)'TRAITEMENT DE LA MAILLE ',k8b
        endif
        if (type(1:4) .eq. 'SEG3') then
            call jeveuo(jexnum(connex, zi(jnuma+i-1)), 'L', jpoin)
            n1=zi(jpoin)
            n2=zi(jpoin+1)
        else if (type(1:4).eq.'POI1') then
            call jeveuo(jexnum(connex, zi(jnuma+i-1)), 'L', jpoin)
            n1=zi(jpoin)
            n2=0
        else
            call utmess('F', 'ALGORITH6_17')
        endif
!
        do 130 j = 1, nbmat
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtyma-1+j)), type)
            call jeveuo(jexnum(connex, j), 'L', jpoin)
!
! --------- TRIA6 , TRIA7
            if (type .eq. 'TRIA6' .or. type .eq. 'TRIA7') then
                nbno=3
                do 20 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        if (n2 .eq. 0) then
                            lmodi=.true.
                            call bartri(i1, n2, zr(jcoor), zi(jpoin))
                            if (niv .eq. 2) then
                                call jenuno(jexnum(nommai, j), k8b)
                                write (ifm,*)'   MAILLE MODIFIEE ',k8b
                            endif
                            goto 130
!
                        else
                            do 10 i2 = 1, nbno
                                if (zi(jpoin+i2-1) .eq. n2) then
                                    lmodi=.true.
                                    call bartri(i1, i2, zr(jcoor), zi( jpoin))
                                    if (niv .eq. 2) then
                                        call jenuno(jexnum(nommai, j), k8b)
                                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                                    endif
                                    goto 130
!
                                endif
10                          continue
                        endif
                    endif
20              continue
!
! --------- QUAD8 , QUAD9
            else if (type.eq.'QUAD8' .or. type.eq.'QUAD9') then
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                nbno=4
                do 40 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        if (n2 .eq. 0) then
                            lmodi=.true.
                            call barqua(i1, n2, zr(jcoor), zi(jpoin))
                            if (niv .eq. 2) then
                                call jenuno(jexnum(nommai, j), k8b)
                                write (ifm,*)'   MAILLE MODIFIEE ',k8b
                            endif
                            goto 130
!
                        else
                            do 30 i2 = 1, nbno
                                if (zi(jpoin+i2-1) .eq. n2) then
                                    lmodi=.true.
                                    call barqua(i1, i2, zr(jcoor), zi( jpoin))
                                    if (niv .eq. 2) then
                                        call jenuno(jexnum(nommai, j), k8b)
                                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                                    endif
                                    goto 130
!
                                endif
30                          continue
                        endif
                    endif
40              continue
!
! --------- TETRA10
            else if (type.eq.'TETRA10') then
                nbno=4
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 60 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        if (n2 .eq. 0) then
                            lmodi=.true.
                            call bartet(i1, n2, zr(jcoor), zi(jpoin))
                            if (niv .eq. 2) then
                                call jenuno(jexnum(nommai, j), k8b)
                                write (ifm,*)'   MAILLE MODIFIEE ',k8b
                            endif
                            goto 130
!
                        else
                            do 50 i2 = 1, nbno
                                if (zi(jpoin+i2-1) .eq. n2) then
                                    lmodi=.true.
                                    call bartet(i1, i2, zr(jcoor), zi( jpoin))
                                    if (niv .eq. 2) then
                                        call jenuno(jexnum(nommai, j), k8b)
                                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                                    endif
                                    goto 130
!
                                endif
50                          continue
                        endif
                    endif
60              continue
!
! --------- PENTA15 , PENTA18
            else if (type.eq.'PENTA15' .or. type.eq.'PENTA18') then
                nbno=6
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 80 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        if (n2 .eq. 0) then
                            lmodi=.true.
                            call barpen(i1, n2, zr(jcoor), zi(jpoin))
                            if (niv .eq. 2) then
                                call jenuno(jexnum(nommai, j), k8b)
                                write (ifm,*)'   MAILLE MODIFIEE ',k8b
                            endif
                            goto 130
!
                        else
                            do 70 i2 = 1, nbno
                                if (zi(jpoin+i2-1) .eq. n2) then
                                    lmodi=.true.
                                    call barpen(i1, i2, zr(jcoor), zi( jpoin))
                                    if (niv .eq. 2) then
                                        call jenuno(jexnum(nommai, j), k8b)
                                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                                    endif
                                    goto 130
!
                                endif
70                          continue
                        endif
                    endif
80              continue
!
! --------- PYRAM13
            else if (type.eq.'PYRAM13') then
                nbno=5
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 100 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        if (n2 .eq. 0) then
                            lmodi=.true.
                            call barpyr(i1, n2, zr(jcoor), zi(jpoin))
                            if (niv .eq. 2) then
                                call jenuno(jexnum(nommai, j), k8b)
                                write (ifm,*)'   MAILLE MODIFIEE ',k8b
                            endif
                            goto 130
!
                        else
                            do 90 i2 = 1, nbno
                                if (zi(jpoin+i2-1) .eq. n2) then
                                    lmodi=.true.
                                    call barpyr(i1, i2, zr(jcoor), zi( jpoin))
                                    if (niv .eq. 2) then
                                        call jenuno(jexnum(nommai, j), k8b)
                                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                                    endif
                                    goto 130
!
                                endif
90                          continue
                        endif
                    endif
100              continue
!
! --------- HEXA20 , HEXA27
            else if (type.eq.'HEXA20' .or. type.eq.'HEXA27') then
                nbno=8
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 120 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        if (n2 .eq. 0) then
                            lmodi=.true.
                            call barhex(i1, n2, zr(jcoor), zi(jpoin))
                            if (niv .eq. 2) then
                                call jenuno(jexnum(nommai, j), k8b)
                                write (ifm,*)'   MAILLE MODIFIEE ',k8b
                            endif
                            goto 130
!
                        else
                            do 110 i2 = 1, nbno
                                if (zi(jpoin+i2-1) .eq. n2) then
                                    lmodi=.true.
                                    call barhex(i1, i2, zr(jcoor), zi( jpoin))
                                    if (niv .eq. 2) then
                                        call jenuno(jexnum(nommai, j), k8b)
                                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                                    endif
                                    goto 130
!
                                endif
110                          continue
                        endif
                    endif
120              continue
!
            else
!
            endif
!
130      continue
!
140  end do
!
    call jedetr(nomjv)
    call jedetr('&&MOMABA_MAILLE')
!
150  continue
!     ------------------------------------------------------------------
!
! --- LECTURE DE LA LISTE DE NOEUDS
!
    call jeveuo(mailla//'.COORDO    .VALE', 'L', jconm)
    call jelira(mailla//'.COORDO    .VALE', 'LONMAX', ndim)
!
!     ON STOCKE LES COORDONNEES DES NOEUDS DU FOND DE FISSURE AVANT
!     LEURS MODIFICATIONS
    call wkvect('&&COORD_NOEUDS', 'V V R', ndim, jcon)
    do 160 i = 1, ndim
        zr(jcon+i-1)=zr(jconm+i-1)
160  end do
!
    motcle(1)='GROUP_NO_FOND'
    tymocl(1)='GROUP_NO'
    motcle(2)='NOEUD_FOND'
    tymocl(2)='NOEUD'
    nomjv='&&MOMABA.LISTE_NO'
    call reliem(' ', mailla, 'NU_NOEUD', 'MODI_MAILLE', 1,&
                nbmc, motcle, tymocl, nomjv, nbma)
    if (nbma .eq. 0) goto 260
!
!     ON VERIFIE L'UNICITE DU NOEUD DU FOND DE FISSURE POUR UN
!     MAILLAGE DE DIMENSION 2
    if (zi(jdim+5) .eq. 2 .and. nbma .gt. 1) then
        call utmess('F', 'ALGORITH6_18')
    endif
!
    call jeveuo(nomjv, 'L', jnuma)
    call wkvect('&&NOEU_MIL_FISS', 'V V I', nbma, jnbma)
!
! --- TRAITEMENT DES NOEUDS
!
    ncount=0
    do 240 i = 1, nbma
        n1=zi(jnuma+i-1)
        n2=0
        if (niv .eq. 2) then
            call jenuno(jexnum(nomnoe, zi(jnuma+i-1)), k8b)
            write (ifm,*)'TRAITEMENT DU NOEUD ',k8b
        endif
        lnmf=.true.
        do 230 j = 1, nbmat
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtyma-1+j)), type)
            call jeveuo(jexnum(connex, j), 'L', jpoin)
!
! --------- TRIA6 , TRIA7
            if (type .eq. 'TRIA6' .or. type .eq. 'TRIA7') then
                nbno=3
                do 170 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        lnmf=.false.
                        lmodi=.true.
                        call bartri(i1, n2, zr(jcoor), zi(jpoin))
                        if (niv .eq. 2) then
                            call jenuno(jexnum(nommai, j), k8b)
                            write (ifm,*)'   MAILLE MODIFIEE ',k8b
                        endif
                        goto 230
!
                    endif
170              continue
!
! --------- QUAD8 , QUAD9
            else if (type.eq.'QUAD8' .or. type.eq.'QUAD9') then
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                nbno=4
                do 180 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        lnmf=.false.
                        lmodi=.true.
                        call barqua(i1, n2, zr(jcoor), zi(jpoin))
                        if (niv .eq. 2) then
                            call jenuno(jexnum(nommai, j), k8b)
                            write (ifm,*)'   MAILLE MODIFIEE ',k8b
                        endif
                        goto 230
!
                    endif
180              continue
!
! --------- TETRA10
            else if (type.eq.'TETRA10') then
                nbno=4
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 190 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        lnmf=.false.
                        lmodi=.true.
                        call bartet(i1, n2, zr(jcoor), zi(jpoin))
                        if (niv .eq. 2) then
                            call jenuno(jexnum(nommai, j), k8b)
                            write (ifm,*)'   MAILLE MODIFIEE ',k8b
                        endif
                        goto 230
!
                    endif
190              continue
!
! --------- PENTA15
            else if (type.eq.'PENTA15') then
                nbno=6
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 200 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        lnmf=.false.
                        lmodi=.true.
                        call barpen(i1, n2, zr(jcoor), zi(jpoin))
                        if (niv .eq. 2) then
                            call jenuno(jexnum(nommai, j), k8b)
                            write (ifm,*)'   MAILLE MODIFIEE ',k8b
                        endif
                        goto 230
!
                    endif
200              continue
!
! --------- PYRAM13
            else if (type.eq.'PYRAM13') then
                nbno=5
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 210 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        lnmf=.false.
                        lmodi=.true.
                        call barpyr(i1, n2, zr(jcoor), zi(jpoin))
                        if (niv .eq. 2) then
                            call jenuno(jexnum(nommai, j), k8b)
                            write (ifm,*)'   MAILLE MODIFIEE ',k8b
                        endif
                        goto 230
!
                    endif
210              continue
!
! --------- HEXA20 , HEXA27
            else if (type.eq.'HEXA20' .or. type.eq.'HEXA27') then
                nbno=8
                call jeveuo(jexnum(connex, j), 'L', jpoin)
                do 220 i1 = 1, nbno
                    if (zi(jpoin+i1-1) .eq. n1) then
                        lnmf=.false.
                        lmodi=.true.
                        call barhex(i1, n2, zr(jcoor), zi(jpoin))
                        if (niv .eq. 2) then
                            call jenuno(jexnum(nommai, j), k8b)
                            write (ifm,*)'   MAILLE MODIFIEE ',k8b
                        endif
                        goto 230
!
                    endif
220              continue
!
            else
!
            endif
!
230      continue
!
        if (lnmf) then
!         ON STOCKE LES NOEUDS MILIEU DU FOND DE FISSURE
            zi(jnbma+ncount)=n1
            ncount=ncount+1
        endif
!
240  end do
!
    do 250 i = 1, ncount
!       ON REAJUSTE LES COORDONNEES DES NOEUDS MILIEU
!       DU FOND DE FISSURE
        nn=3*(zi(jnbma+i-1)-1)
        zr(jcoor+nn)=zr(jcon+nn)
        zr(jcoor+nn+1)=zr(jcon+nn+1)
        zr(jcoor+nn+2)=zr(jcon+nn+2)
250  end do
!
    if (.not.lmodi) then
        call utmess('F', 'ALGORITH16_72')
    endif
!
    call jedetr('&&NOEU_MIL_FISS')
    call jedetr('&&COORD_NOEUDS')
    call jedetr(nomjv)
    call jedetr('&&MOMABA_MAILLE')
!
260  continue
!
    call jedema()
end subroutine
