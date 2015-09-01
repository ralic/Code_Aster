subroutine pj3dtr(cortr3, corres, nutm3d, elrf3d, geom1,&
                  geom2, dala)
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

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/elraca.h"
#include "asterfort/elrfvf.h"
#include "asterfort/getvtx.h"
#include "asterfort/indiis.h"
#include "asterfort/inslri.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/pjeflo.h"
#include "asterfort/pjefmi.h"
#include "asterfort/pjloin.h"
#include "asterfort/reereg.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

    character(len=16), intent(in) :: corres, cortr3
    character(len=8), intent(in) :: elrf3d(10)
    integer, intent(in) :: nutm3d(10)
    real(kind=8), intent(in) :: geom1(*), geom2(*)
    real(kind=8), intent(in) :: dala
! ----------------------------------------------------------------------
!  but :
!    transformer cortr3 en corres en utilisant les fonc. de forme
!    des mailles du maillage1 (en 3d isoparametrique)

!  in/jxin   cortr3    k16 : nom du corresp_2_mailla fait avec les tetr4
!  in/jxout  corres    k16 : nom du corresp_2_mailla final
!  in        nutm3d(10) i  : numeros des 10 types de mailles 3d
!  in        elrf3d(10) k8 : noms des 10 types de mailles 3d
! ----------------------------------------------------------------------

    aster_logical :: lext
    integer :: nbnomx, nbfamx
    parameter    ( nbnomx=27, nbfamx=20)
    integer :: cntetr(4, 1), cnpent(4, 3), cnhexa(4, 6), cnpyra(4, 2)
    integer :: nbpg(nbfamx)
    real(kind=8) :: ksi, eta, dzeta, x1, x2, x3, vol
    real(kind=8) :: crrefe(3*nbnomx), xr1(3), xr2(3), xr3(3)
    real(kind=8) :: ff(nbnomx), cooele(3*nbnomx)
    character(len=8) :: elrefa, m1, m2, fapg(nbfamx)
    integer :: i1conb, i1conu, nno1, nno2
    integer :: nma1, nma2, ialim1, ialin1, ialin2, ilcnx1
    integer :: j2xxk1, i2conb, i2com1, ideca2, ino2, itr, ima1, nbno, i2conu
    integer :: i2cocf, i2coco, ideca1, itypm, nutm, ityp, ndim, nno, kdim
    integer :: nnos, nbfpg, kk, ino, nuno, iret

    integer :: nbmax
    parameter  (nbmax=5)
    integer :: tino2m(nbmax), nbnod, nbnodm
    real(kind=8) :: tdmin2(nbmax), disprj, distv
    aster_logical :: loin2
    integer, pointer :: typmail(:) => null()
    character(len=24), pointer :: pjxx_k1(:) => null()
    real(kind=8), pointer :: pjef_cf(:) => null()
    integer, pointer :: tetr4(:) => null()
    integer, pointer :: pjef_tr(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: lino_loin(:) => null()
! --- DEB --------------------------------------------------------------

    call jemarq()

!   0. DECOUPAGE DES ELEMENTS 3D EN TETRA (VOIR PJ3DC0) :
!   ----------------------------------------------------

!     0.1 : TETRAEDRE :
!     -----------------
    cntetr(1,1)=1
    cntetr(2,1)=2
    cntetr(3,1)=3
    cntetr(4,1)=4

!     0.2 : PENTAEDRE :
!     -----------------
    cnpent(1,1)=1
    cnpent(2,1)=3
    cnpent(3,1)=6
    cnpent(4,1)=5

    cnpent(1,2)=1
    cnpent(2,2)=6
    cnpent(3,2)=4
    cnpent(4,2)=5

    cnpent(1,3)=1
    cnpent(2,3)=3
    cnpent(3,3)=5
    cnpent(4,3)=2

!     0.3 : HEXAEDRE :
!     -----------------
    cnhexa(1,1)=1
    cnhexa(2,1)=4
    cnhexa(3,1)=8
    cnhexa(4,1)=6

    cnhexa(1,2)=1
    cnhexa(2,2)=8
    cnhexa(3,2)=6
    cnhexa(4,2)=5

    cnhexa(1,3)=1
    cnhexa(2,3)=4
    cnhexa(3,3)=6
    cnhexa(4,3)=2

    cnhexa(1,4)=2
    cnhexa(2,4)=4
    cnhexa(3,4)=8
    cnhexa(4,4)=7

    cnhexa(1,5)=2
    cnhexa(2,5)=8
    cnhexa(3,5)=6
    cnhexa(4,5)=7

    cnhexa(1,6)=2
    cnhexa(2,6)=4
    cnhexa(3,6)=7
    cnhexa(4,6)=3

!     0.4 : PYRAMIDE :
!     -----------------
    cnpyra(1,1)=1
    cnpyra(2,1)=2
    cnpyra(3,1)=3
    cnpyra(4,1)=5

    cnpyra(1,2)=1
    cnpyra(2,2)=3
    cnpyra(3,2)=4
    cnpyra(4,2)=5


!   1. RECUPERATION DES INFORMATIONS GENERALES :
!   -----------------------------------------------
    call jeveuo(cortr3//'.PJXX_K1', 'L', vk24=pjxx_k1)
    call jeveuo(cortr3//'.PJEF_NB', 'L', i1conb)
    call jeveuo(cortr3//'.PJEF_NU', 'L', i1conu)
    call jeveuo(cortr3//'.PJEF_CF', 'L', vr=pjef_cf)
    call jeveuo(cortr3//'.PJEF_TR', 'L', vi=pjef_tr)

    m1=pjxx_k1(1)(1:8)
    m2=pjxx_k1(2)(1:8)
    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', m1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', m2, 'MAILLAGE', repi=nma2)

    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)
    call jeveuo('&&PJXXCO.TETR4', 'L', vi=tetr4)

    call jeveuo(m1//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jeveuo(m1//'.TYPMAIL', 'L', vi=typmail)

!   -- l'objet lino_loin contiendra la liste des noeuds projetes un peu loin
    AS_ALLOCATE(vi=lino_loin, size=nno2)


!   2. ALLOCATION DE CORRES :
!   -----------------------------------------------
    call wkvect(corres//'.PJXX_K1', 'V V K24', 5, j2xxk1)
    zk24(j2xxk1-1+1)=m1
    zk24(j2xxk1-1+2)=m2
    zk24(j2xxk1-1+3)='COLLOCATION'

!     2.1 REMPLISSAGE DE .PJEF_NB ET .PJEF_M1:
!     -----------------------------------------
    call wkvect(corres//'.PJEF_NB', 'V V I', nno2, i2conb)
    call wkvect(corres//'.PJEF_M1', 'V V I', nno2, i2com1)
    ideca2=0
    do ino2 = 1, nno2
!       ITR : TETR4 ASSOCIE A INO2
        itr=pjef_tr(ino2)
        if (itr .eq. 0) cycle
!       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
        ima1=tetr4(1+6*(itr-1)+5)
        nbno=zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)
        zi(i2conb-1+ino2)=nbno
        zi(i2com1-1+ino2)=ima1
        ideca2=ideca2+nbno
    enddo
    if (ideca2 .eq. 0) then
        call utmess('F', 'CALCULEL3_97')
    endif

    loin2 = .false.
    nbnod = 0
    tdmin2(:)=0.d0
    tino2m(:)=0
    nbnodm = 0


!   2.2 allocation de .pjef_nu .pjef_cf .pjef_co:
!       (et remplissage de ces 3 objets)
!   ------------------------------------------------------
    call wkvect(corres//'.PJEF_NU', 'V V I', ideca2, i2conu)
    call wkvect(corres//'.PJEF_CF', 'V V R', ideca2, i2cocf)
    call wkvect(corres//'.PJEF_CO', 'V V R', 3*nno2, i2coco)
    ideca1=0
    ideca2=0
    do ino2 = 1, nno2
!       ITR : TETR4 ASSOCIE A INO2
        itr = pjef_tr(ino2)
        if (itr .eq. 0) cycle
!       IMA1 : MAILLE DE M1 ASSOCIE AU TETR4 ITR
        ima1 = tetr4(1+6*(itr-1)+5)
!       ITYPM : TYPE DE LA MAILLE IMA1
        itypm = typmail(ima1)
        nutm = indiis(nutm3d,itypm,1,10)
        elrefa = elrf3d(nutm)
        ityp = tetr4(1+6*(itr-1)+6)
        nbno = zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)

        call elraca(elrefa, ndim, nno, nnos, nbfpg,&
                    fapg, nbpg, crrefe, vol)

        ASSERT(nbno .eq. nno)

!       2.2.1 determination des coordonnees de ino2 dans l'element
!             de reference : ksi , eta et dzeta
!       -----------------------------------------------------------
        ksi =0.d0
        eta =0.d0
        dzeta=0.d0

        if (elrefa .eq. 'TE4' .or. elrefa .eq. 'T10') then
            do kk = 1, 4
                x1 = crrefe(ndim*(cntetr(kk,ityp)-1)+1)
                x2 = crrefe(ndim*(cntetr(kk,ityp)-1)+2)
                x3 = crrefe(ndim*(cntetr(kk,ityp)-1)+3)
                ksi = ksi + pjef_cf(ideca1+kk)*x1
                eta = eta + pjef_cf(ideca1+kk)*x2
                dzeta = dzeta + pjef_cf(ideca1+kk)*x3
            enddo

        else if (elrefa.eq.'PE6' .or. elrefa.eq.'P15'.or. elrefa.eq.'P18' ) then
            do kk = 1, 4
                x1 = crrefe(ndim*(cnpent(kk,ityp)-1)+1)
                x2 = crrefe(ndim*(cnpent(kk,ityp)-1)+2)
                x3 = crrefe(ndim*(cnpent(kk,ityp)-1)+3)
                ksi = ksi + pjef_cf(ideca1+kk)*x1
                eta = eta + pjef_cf(ideca1+kk)*x2
                dzeta = dzeta + pjef_cf(ideca1+kk)*x3
            enddo

        else if (elrefa.eq.'HE8' .or. elrefa.eq.'H20' .or. elrefa.eq.'H27' ) then
            do kk = 1, 4
                x1 = crrefe(ndim*(cnhexa(kk,ityp)-1)+1)
                x2 = crrefe(ndim*(cnhexa(kk,ityp)-1)+2)
                x3 = crrefe(ndim*(cnhexa(kk,ityp)-1)+3)
                ksi = ksi + pjef_cf(ideca1+kk)*x1
                eta = eta + pjef_cf(ideca1+kk)*x2
                dzeta = dzeta + pjef_cf(ideca1+kk)*x3
            enddo

        else if (elrefa.eq.'PY5' .or. elrefa.eq.'P13') then
            do kk = 1, 4
                x1 = crrefe(ndim*(cnpyra(kk,ityp)-1)+1)
                x2 = crrefe(ndim*(cnpyra(kk,ityp)-1)+2)
                x3 = crrefe(ndim*(cnpyra(kk,ityp)-1)+3)
                ksi = ksi + pjef_cf(ideca1+kk)*x1
                eta = eta + pjef_cf(ideca1+kk)*x2
                dzeta = dzeta + pjef_cf(ideca1+kk)*x3
            enddo

        else
            call utmess('F', 'ELEMENTS_55', sk=elrefa)
        endif

        xr1(1) = ksi
        xr1(2) = eta
        xr1(3) = dzeta

!       -- on essaye d'ameliorer la precision de xr1(*) en utilisant reereg :
        do ino = 1, nbno
            nuno = connex(1+ zi(ilcnx1-1+ima1)-2+ino)
            do kdim = 1, ndim
                cooele(ndim*(ino-1)+kdim)=geom1(3*(nuno-1)+kdim)
            enddo
        enddo
        call reereg('C', elrefa, nno, cooele, geom2(3*(ino2-1)+1),&
                    ndim, xr2, iret)

!       -- on regarde si ino2 est exterieur a ima1 :
        call pjeflo(elrefa, ndim, iret, xr2, disprj)
        lext= (disprj.gt.1.0d-02)

!       -- on choisit la meilleure approximation entre xr1 et xr2:
        call pjefmi(elrefa, nno, cooele, geom2(3*(ino2-1)+1), ndim,&
                    xr1, xr2, lext, xr3, distv)

        if (distv.lt.dala) then
            lext=.false.
        else
            if (nint(disprj) .eq. 999) then
                loin2=.true.
            else if (disprj .gt. 1.0d-01) then
                loin2=.true.
                nbnodm = nbnodm + 1
                lino_loin(nbnodm)=ino2
                call inslri(nbmax, nbnod, tdmin2, tino2m, distv,ino2)
            endif
        endif

        zr(i2coco-1+3*(ino2-1)+1)=xr3(1)
        zr(i2coco-1+3*(ino2-1)+2)=xr3(2)
        zr(i2coco-1+3*(ino2-1)+3)=xr3(3)

!       2.2.2 :
!       calcul des f. de forme aux noeuds pour le point ksi,eta,dzeta:
!       --------------------------------------------------------------
        call elrfvf(elrefa, xr3, 27, ff, nno)

        do ino = 1, nbno
            nuno = connex(1+ zi(ilcnx1-1+ima1)-2+ino)
            zi(i2conu-1+ideca2+ino) = nuno
            zr(i2cocf-1+ideca2+ino) = ff(ino)
        enddo

        ideca1=ideca1+4
        ideca2=ideca2+nbno
    enddo

!   emission d'un eventuel message d'alarme:
    if (loin2) then
        call pjloin(nbnod,nbnodm,m2,geom2,nbmax,tino2m,tdmin2,lino_loin)
    endif

    AS_DEALLOCATE(vi=lino_loin)

    call jedema()
end subroutine
