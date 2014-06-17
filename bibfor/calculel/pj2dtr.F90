subroutine pj2dtr(cortr3, corres, nutm2d, elrf2d, geom1,&
                  geom2, lraff)
    implicit none
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
#include "asterfort/reereg.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: corres, cortr3
    integer :: nbtm
    parameter    (nbtm=6)
    character(len=8) :: elrf2d(nbtm)
    integer :: nutm2d(nbtm)
    real(kind=8) :: geom1(*), geom2(*)
    logical :: lraff
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     BUT :
!       TRANSFORMER CORTR3 EN CORRES EN UTILISANT LES FONC. DE FORME
!       DES MAILLES DU MAILLAGE1 (EN 2D ISOPARAMETRIQUE)
!
!
!  IN/JXIN   CORTR3   K16 : NOM DU CORRESP_2_MAILLA FAIT AVEC LES TRIA3
!  IN/JXOUT  CORRES   K16 : NOM DU CORRESP_2_MAILLA FINAL
!  IN        NUTM2D(6) I  : NUMEROS DES 6 TYPES DE MAILLES 2D
!  IN        ELRF2D(6) K8 : NOMS DES 6 TYPES DE MAILLES 2D
!  IN        GEOM1        : geometrie des noeuds du maillage 1
!  IN        GEOM2        : GEOMETRIE DES NOEUDS DU MAILLAGE 2
!  IN        LRAFF     L  : .TRUE. => ON VA UTILISER REEREG.F
!                            POUR ESSAYER DE "RAFFINER" LA PRECISION
!                            INUTILISABLE DANS LE CAS 2.5D
! ----------------------------------------------------------------------
!
    logical :: lext
    integer :: nbnomx, nbfamx
    parameter    ( nbnomx=27, nbfamx=20)
    character(len=8) :: m1, m2, elrefa, fapg(nbfamx), alarme
    character(len=16) :: k16bid, nomcmd
    integer :: nbpg(nbfamx), cnquad(3, 2)
    real(kind=8) :: crrefe(3*nbnomx), ksi, eta, xr1(2), xr2(2), xr3(2)
    real(kind=8) :: ff(nbnomx), cooele(3*nbnomx), x1, x2, vol
    integer ::  i1conb,  i1conu,  i2cocf, i2coco
    integer :: i2com1, i2conb, j2xxk1, i2conu,  ialim1, ialin1, ialin2
    integer ::   ideca1, ideca2, ilcnx1, ima1, ino, ino2
    integer :: iret, itr, itypm, kdim, kk, nbfpg, nbno, ndim, nma1, nma2, nno
    integer :: nno1, nno2, nnos, nuno, nuno2, nutm, ibid
!
    integer :: nbmax
    parameter  (nbmax=5)
    integer :: tino2m(nbmax), nbnod, nbnodm, ii, ino2m
    real(kind=8) :: tdmin2(nbmax), umessr(4), disprj, distv
    character(len=8) :: nono2
    logical :: loin2
    character(len=24), pointer :: pjxx_k1(:) => null()
    integer, pointer :: pjef_tr(:) => null()
    integer, pointer :: tria3(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: connex(:) => null()
    real(kind=8), pointer :: pjef_cf(:) => null()
! --- DEB --------------------------------------------------------------
!
    call jemarq()
!
!     0. DECOUPAGE DES QUADRANGLES EN 2 TRIANGLES (VOIR PJ2DCO)
!     ----------------------------------------------------------
!
    cnquad(1,1)=1
    cnquad(2,1)=2
    cnquad(3,1)=3
!
    cnquad(1,2)=1
    cnquad(2,2)=3
    cnquad(3,2)=4
!
!     1. RECUPERATION DES INFORMATIONS GENERALES :
!     -----------------------------------------------
    call jeveuo(cortr3//'.PJXX_K1', 'L', vk24=pjxx_k1)
    call jeveuo(cortr3//'.PJEF_NB', 'L', i1conb)
    call jeveuo(cortr3//'.PJEF_NU', 'L', i1conu)
    call jeveuo(cortr3//'.PJEF_CF', 'L', vr=pjef_cf)
    call jeveuo(cortr3//'.PJEF_TR', 'L', vi=pjef_tr)
!
    m1=pjxx_k1(1)(1:8)
    m2=pjxx_k1(2)(1:8)
    call dismoi('NB_NO_MAILLA', m1, 'MAILLAGE', repi=nno1)
    call dismoi('NB_NO_MAILLA', m2, 'MAILLAGE', repi=nno2)
    call dismoi('NB_MA_MAILLA', m1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_MA_MAILLA', m2, 'MAILLAGE', repi=nma2)
!
    call jeveuo('&&PJXXCO.LIMA1', 'L', ialim1)
    call jeveuo('&&PJXXCO.LINO1', 'L', ialin1)
    call jeveuo('&&PJXXCO.LINO2', 'L', ialin2)
    call jeveuo('&&PJXXCO.TRIA3', 'L', vi=tria3)
!
    call jeveuo(m1//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(m1//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jeveuo(m1//'.TYPMAIL', 'L', vi=typmail)
!
!
!     2. ALLOCATION DE CORRES :
!     -----------------------------------------------
    call wkvect(corres//'.PJXX_K1', 'V V K24', 5, j2xxk1)
    zk24(j2xxk1-1+1)=m1
    zk24(j2xxk1-1+2)=m2
    zk24(j2xxk1-1+3)='COLLOCATION'
!
!
!     2.1 REMPLISSAGE DE .PJEF_NB ET .PJEF_M1:
!     -----------------------------------------
    call wkvect(corres//'.PJEF_NB', 'V V I', nno2, i2conb)
    call wkvect(corres//'.PJEF_M1', 'V V I', nno2, i2com1)
    ideca2=0
    do ino2 = 1, nno2
!       ITR : TRIA3 ASSOCIE A INO2
        itr=pjef_tr(ino2)
        if (itr .eq. 0) goto 10
!       IMA1 : MAILLE DE M1 ASSOCIE AU TRIA3 ITR
        ima1=tria3(1+4*(itr-1)+4)
        nbno=zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)
        zi(i2conb-1+ino2)=nbno
        zi(i2com1-1+ino2)=ima1
        ideca2=ideca2+nbno
 10     continue
    enddo
    if (ideca2 .eq. 0) then
        call utmess('F', 'CALCULEL3_97')
    endif
!
    loin2 = .false.
    nbnod = 0
    nbnodm = 0
!     2.2 ALLOCATION DE .PJEF_NU .PJEF_CF .PJEF_CO:
!         (ET REMPLISSAGE DE CES 3 OBJETS)
!     ------------------------------------------------------
    call wkvect(corres//'.PJEF_NU', 'V V I', ideca2, i2conu)
    call wkvect(corres//'.PJEF_CF', 'V V R', ideca2, i2cocf)
    call wkvect(corres//'.PJEF_CO', 'V V R', 3*nno2, i2coco)
    ideca1=0
    ideca2=0
    do ino2 = 1, nno2
!       ITR : TRIA3 ASSOCIE A INO2
        itr = pjef_tr(ino2)
        if (itr .eq. 0) goto 20
!       IMA1 : MAILLE DE M1 ASSOCIE AU TRIA3 ITR
        ima1= tria3(1+4*(itr-1)+4)
!       ITYPM : TYPE DE LA MAILLE IMA1
        itypm = typmail(ima1)
        nutm = indiis(nutm2d,itypm,1,nbtm)
        elrefa = elrf2d(nutm)
        nbno = zi(ilcnx1+ima1)-zi(ilcnx1-1+ima1)
!
        call elraca(elrefa, ndim, nno, nnos, nbfpg,&
                    fapg, nbpg, crrefe, vol)
!
        ASSERT(nbno .eq. nno)
!
!       2.2.1 DETERMINATION DES COORDONEES DE INO2 DANS L'ELEMENT
!             DE REFERENCE : KSI , ETA
!     -----------------------------------------------------------
        ksi=0.d0
        eta=0.d0
!       -- NUMERO DU 2EME NOEUD DE IMA1 : NUNO2
        nuno2=connex(1+ zi(ilcnx1-1+ima1)-2+2)
!       SI NUNO2 EST IDENTIQUE AU 2EME NOEUD DU TRIA3
!       C'EST QUE LE TRIA3 EST EN "DESSOUS" :
!
        if (elrefa .eq. 'TR3' .or. elrefa .eq. 'TR6' .or. elrefa .eq. 'TR7') then
!
            do kk = 1, 3
                x1 = crrefe(ndim*(kk-1)+1)
                x2 = crrefe(ndim*(kk-1)+2)
                ksi = ksi + pjef_cf(ideca1+kk)*x1
                eta = eta + pjef_cf(ideca1+kk)*x2
            enddo
!
            elseif (elrefa.eq.'QU4' .or. elrefa.eq.'QU8' .or.&
                                     elrefa.eq.'QU9' ) then
            if (nuno2 .eq. tria3(1+4*(itr-1)+2)) then
!         -- SI 1ER TRIANGLE :
                do kk = 1, 3
                    x1 = crrefe(ndim*(cnquad(kk,1)-1)+1)
                    x2 = crrefe(ndim*(cnquad(kk,1)-1)+2)
                    ksi = ksi + pjef_cf(ideca1+kk)*x1
                    eta = eta + pjef_cf(ideca1+kk)*x2
                enddo
            else
!         -- SI 2EME TRIANGLE :
                do kk = 1, 3
                    x1 = crrefe(ndim*(cnquad(kk,2)-1)+1)
                    x2 = crrefe(ndim*(cnquad(kk,2)-1)+2)
                    ksi = ksi + pjef_cf(ideca1+kk)*x1
                    eta = eta + pjef_cf(ideca1+kk)*x2
                enddo
            endif
!
        else
            call utmess('F', 'ELEMENTS_55', sk=elrefa)
        endif
!
        xr1(1) = ksi
        xr1(2) = eta
!
        if (lraff) then
!         -- on essaye d'ameliorer la precision de xr1(*) en utilisant reereg :
            do ino = 1, nbno
                nuno = connex(1+ zi(ilcnx1-1+ima1)-2+ino)
                do kdim = 1, ndim
                    cooele(ndim*(ino-1)+kdim)=geom1(3*(nuno-1)+kdim)
                enddo
            enddo
            call reereg('C', elrefa, nno, cooele, geom2(3*(ino2-1)+1),&
                        ndim, xr2, iret)
!
!           -- on regarde si ino2 est exterieur a ima1 :
            call pjeflo(elrefa, ndim, iret, xr2, disprj)
            lext= (disprj.gt.1.0d-02)
!
!         -- on choisit la meilleure approximation entre xr1 et xr2:
            call pjefmi(elrefa, nno, cooele, geom2(3*(ino2-1)+1), ndim,&
                        xr1, xr2, lext, xr3, distv)

            if (nint(disprj).eq.999) then
                loin2=.true.
            else if (disprj .gt. 1.0d-01) then
                loin2=.true.
                nbnodm = nbnodm + 1
                call inslri(nbmax, nbnod, tdmin2, tino2m, distv,ino2)
            endif

        else
            xr3(1)=xr1(1)
            xr3(2)=xr1(2)
        endif
!
        zr(i2coco-1+3*(ino2-1)+1)=xr3(1)
        zr(i2coco-1+3*(ino2-1)+2)=xr3(2)
!
!       2.2.2 :
!       calcul des f. de forme aux noeuds pour le point xr3
!       ----------------------------------------------------
        call elrfvf(elrefa, xr3, 27, ff, nno)
        do ino = 1, nbno
            nuno = connex(1+ zi(ilcnx1-1+ima1)-2+ino)
            zi(i2conu-1+ideca2+ino) = nuno
            zr(i2cocf-1+ideca2+ino) = ff(ino)
        enddo
!
        ideca1=ideca1+3
        ideca2=ideca2+nbno
!
 20     continue
    enddo
!
!   -- emission d'un eventuel message d'alarme:
!   --------------------------------------------
    if (loin2) then
        alarme='OUI'
        call getres(k16bid, k16bid, nomcmd)
        if (nomcmd .eq. 'PROJ_CHAMP') then
            call getvtx(' ', 'ALARME', scal=alarme, nbret=ibid)
        endif
        if (alarme .eq. 'OUI') then
            do ii = 1, nbnod
                ino2m = tino2m(ii)
                call jenuno(jexnum(m2//'.NOMNOE', ino2m), nono2)
                umessr(1) = geom2(3*(ino2m-1)+1)
                umessr(2) = geom2(3*(ino2m-1)+2)
                umessr(3) = geom2(3*(ino2m-1)+3)
                umessr(4) = tdmin2(ii)
                call utmess('I', 'CALCULEL5_43', sk=nono2, nr=4, valr=umessr)
            enddo
            call utmess('A', 'CALCULEL5_48',si=nbnodm )
        endif
    endif
!
    call jedema()
end subroutine
