subroutine momaba(mailla)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/barhex.h"
#include "asterfort/barpen.h"
#include "asterfort/barpyr.h"
#include "asterfort/barqua.h"
#include "asterfort/bartet.h"
#include "asterfort/bartri.h"
#include "asterfort/cncinv.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mailla
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
    integer :: jtyma, nbmc, nbma, jnuma, i, ityp, n1, n2, i1, nbno
    integer :: nbmat, jpoin, ifm, niv, jcon, ndim, nn, jnbma, ncount
    integer :: ilcnx2, nbm1, kk, numa, i1sauv, i2sauv
    aster_logical :: lnmf, lmodi, lconx
    parameter(nbmc=2)
    character(len=8) :: k8b, type
    character(len=16) :: tymocl(nbmc), motcle(nbmc)
    character(len=24) :: connex, nommai, nomnoe, nomjv
    integer, pointer :: dime(:) => null()
    integer, pointer :: coninv(:) => null()
    real(kind=8), pointer :: conm(:) => null()
    real(kind=8), pointer :: coor(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    connex=mailla//'.CONNEX'
    nommai=mailla//'.NOMMAI'
    nomnoe=mailla//'.NOMNOE'
    call jeveuo(mailla//'.TYPMAIL        ', 'L', jtyma)
    call jeveuo(mailla//'.COORDO    .VALE', 'E', vr=coor)
    call jeveuo(mailla//'.DIME           ', 'L', vi=dime)
    call dismoi('NB_MA_MAILLA', mailla, 'MAILLAGE', repi=nbmat)
    lmodi=.false.
    lconx=.false.
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
    if (dime(6) .eq. 2) then
        call utmess('F', 'ALGORITH6_16')
    endif
!
! --- CREATION DE LA CONNECTIVITE INVERSE
!
    call cncinv(mailla, [0], 0, 'V', '&&MOMABA.CONINV')
    call jeveuo('&&MOMABA.CONINV', 'L', vi=coninv)
    call jeveuo(jexatr('&&MOMABA.CONINV', 'LONCUM'), 'L', ilcnx2)
    lconx=.true.
!
    do i = 1, nbma
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
        nbm1 = zi(ilcnx2+n1)-zi(ilcnx2-1+n1)
!
! ----- BOUCLE SUR LES MAILLES AUXQUELLES SONT LIEES CE NOEUD
        do kk = 1, nbm1
            numa = coninv(1+zi(ilcnx2-1+n1)-1+kk-1)
            call jeveuo(jexnum(connex, numa), 'L', jpoin)
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtyma-1+numa)), type)
!
            nbno=0
! --------- TRIA6, TRIA7
            if (type .eq. 'TRIA6' .or. type .eq. 'TRIA7') then
                nbno=3
! --------- QUAD8, QUAD9
            else if (type.eq.'QUAD8' .or. type.eq.'QUAD9') then
                nbno=4
! --------- TETRA10
            else if (type.eq.'TETRA10') then
                nbno=4
! --------- PENTA15, PENTA18
            else if (type.eq.'PENTA15' .or. type.eq.'PENTA18') then
                nbno=6
! --------- PYRAM13
            else if (type.eq.'PYRAM13') then
                nbno=5
! --------- HEXA20, HEXA27
            else if (type.eq.'HEXA20' .or. type.eq.'HEXA27') then
                nbno=8
            endif
!
            i1sauv = 0
            i2sauv = 0
            do i1 = 1, nbno
                if (zi(jpoin+i1-1) .eq. n1) i1sauv = i1
                if (zi(jpoin+i1-1) .eq. n2) i2sauv = i1
            enddo
!
            if ( (i1sauv.ne.0.and.i2sauv.ne.0).or.&
                 (i1sauv.ne.0.and.n2.eq.0) ) then
! ------------- TRIA6, TRIA7
                if (type .eq. 'TRIA6' .or. type .eq. 'TRIA7') then
                    lmodi=.true.
                    call bartri(i1sauv, i2sauv, coor, zi( jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- QUAD8, QUAD9
                else if (type.eq.'QUAD8' .or. type.eq.'QUAD9') then
                    lmodi=.true.
                    call barqua(i1sauv, i2sauv, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- TETRA10
                else if (type.eq.'TETRA10') then
                    lmodi=.true.
                    call bartet(i1sauv, i2sauv, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- PENTA15, PENTA18
                else if (type.eq.'PENTA15' .or. type.eq.'PENTA18') then
                    lmodi=.true.
                    call barpen(i1sauv, i2sauv, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- PYRAM13
                else if (type.eq.'PYRAM13') then
                    lmodi=.true.
                    call barpyr(i1sauv, i2sauv, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- HEXA20, HEXA27
                else if (type.eq.'HEXA20' .or. type.eq.'HEXA27') then
                    lmodi=.true.
                    call barhex(i1sauv, i2sauv, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
                endif
            endif
        enddo
!
    end do
!
    call jedetr(nomjv)
    call jedetr('&&MOMABA_MAILLE')
!
150 continue
!     ------------------------------------------------------------------
!
! --- LECTURE DE LA LISTE DE NOEUDS
!
    call jeveuo(mailla//'.COORDO    .VALE', 'L', vr=conm)
    call jelira(mailla//'.COORDO    .VALE', 'LONMAX', ndim)
!
!     ON STOCKE LES COORDONNEES DES NOEUDS DU FOND DE FISSURE AVANT
!     LEURS MODIFICATIONS
    call wkvect('&&COORD_NOEUDS', 'V V R', ndim, jcon)
    do i = 1, ndim
        zr(jcon+i-1)=conm(i)
    end do
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
    if ( .not.lconx ) then
!
! ------- CREATION DE LA CONNECTIVITE INVERSE
!
        call cncinv(mailla, [0], 0, 'V', '&&MOMABA.CONINV')
        call jeveuo('&&MOMABA.CONINV', 'L', vi=coninv)
        call jeveuo(jexatr('&&MOMABA.CONINV', 'LONCUM'), 'L', ilcnx2)
        lconx=.true.
    endif
!
!     ON VERIFIE L'UNICITE DU NOEUD DU FOND DE FISSURE POUR UN
!     MAILLAGE DE DIMENSION 2
    if (dime(6) .eq. 2 .and. nbma .gt. 1) then
        call utmess('F', 'ALGORITH6_18')
    endif
!
    call jeveuo(nomjv, 'L', jnuma)
    call wkvect('&&NOEU_MIL_FISS', 'V V I', nbma, jnbma)
!
! --- TRAITEMENT DES NOEUDS
!
    ncount=0
    do i = 1, nbma
        n1=zi(jnuma+i-1)
        n2=0
        if (niv .eq. 2) then
            call jenuno(jexnum(nomnoe, zi(jnuma+i-1)), k8b)
            write (ifm,*)'TRAITEMENT DU NOEUD ',k8b
        endif
        lnmf=.true.
!
        nbm1 = zi(ilcnx2+n1)-zi(ilcnx2-1+n1)
!
! ----- BOUCLE SUR LES MAILLES AUXQUELLES SONT LIEES CE NOEUD
        do kk = 1, nbm1
            numa = coninv(1+zi(ilcnx2-1+n1)-1+kk-1)
            call jeveuo(jexnum(connex, numa), 'L', jpoin)
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtyma-1+numa)), type)
!
            nbno=0
! --------- TRIA6, TRIA7
            if (type .eq. 'TRIA6' .or. type .eq. 'TRIA7') then
                nbno=3
! --------- QUAD8, QUAD9
            else if (type.eq.'QUAD8' .or. type.eq.'QUAD9') then
                nbno=4
! --------- TETRA10
            else if (type.eq.'TETRA10') then
                nbno=4
! --------- PENTA15, PENTA18
            else if (type.eq.'PENTA15' .or. type.eq.'PENTA18') then
                nbno=6
! --------- PYRAM13
            else if (type.eq.'PYRAM13') then
                nbno=5
! --------- HEXA20, HEXA27
            else if (type.eq.'HEXA20' .or. type.eq.'HEXA27') then
                nbno=8
            endif
!
            i1sauv = 0
            do i1 = 1, nbno
                if (zi(jpoin+i1-1) .eq. n1) i1sauv = i1
            enddo
!
            if ( i1sauv.ne.0 ) then
! ------------- TRIA6, TRIA7
                if (type .eq. 'TRIA6' .or. type .eq. 'TRIA7') then
                    lnmf=.false.
                    lmodi=.true.
                    call bartri(i1sauv, n2, coor, zi( jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- QUAD8, QUAD9
                else if (type.eq.'QUAD8' .or. type.eq.'QUAD9') then
                    lnmf=.false.
                    lmodi=.true.
                    call barqua(i1sauv, n2, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- TETRA10
                else if (type.eq.'TETRA10') then
                    lnmf=.false.
                    lmodi=.true.
                    call bartet(i1sauv, n2, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- PENTA15
                else if (type.eq.'PENTA15') then
                    lnmf=.false.
                    lmodi=.true.
                    call barpen(i1sauv, n2, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- PYRAM13
                else if (type.eq.'PYRAM13') then
                    lnmf=.false.
                    lmodi=.true.
                    call barpyr(i1sauv, n2, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
! ------------- HEXA20, HEXA27
                else if (type.eq.'HEXA20' .or. type.eq.'HEXA27') then
                    lnmf=.false.
                    lmodi=.true.
                    call barhex(i1sauv, n2, coor, zi(jpoin))
                    if (niv .eq. 2) then
                        call jenuno(jexnum(nommai, numa), k8b)
                        write (ifm,*)'   MAILLE MODIFIEE ',k8b
                    endif
                endif
            endif
        enddo
!
        if (lnmf) then
!         ON STOCKE LES NOEUDS MILIEU DU FOND DE FISSURE
            zi(jnbma+ncount)=n1
            ncount=ncount+1
        endif
!
    end do
!
    do i = 1, ncount
!       ON REAJUSTE LES COORDONNEES DES NOEUDS MILIEU
!       DU FOND DE FISSURE
        nn=3*(zi(jnbma+i-1)-1)
        coor(nn+1)=zr(jcon+nn)
        coor(1+nn+1)=zr(jcon+nn+1)
        coor(1+nn+2)=zr(jcon+nn+2)
    end do
!
    if (.not.lmodi) then
        call utmess('F', 'ALGORITH16_72')
    endif
!
    call jedetr('&&NOEU_MIL_FISS')
    call jedetr('&&COORD_NOEUDS')
    call jedetr(nomjv)
    call jedetr('&&MOMABA_MAILLE')
    call jedetr('&&MOMABA.CONINV')
!
260 continue
!
    call jedema()
end subroutine
