subroutine arch93(resu, concep, nume, raide, nbmodd,&
                  nbmodf, nbmoda, nbmoad, nbmodi, nbpsmo)
    implicit none
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
!     ------------------------------------------------------------------
!
!     BUT:
!       OPERATEUR MODE_STATIQUE
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/irecri.h"
#include "asterfort/irparb.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rgndas.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
    integer :: ibid, neq, ifm, niv, lmoad, lmoda, vali, iret, nbmodi, lddld
    integer :: lmodd, lddlf, lmodf, lvale, nive, versio, ind, ie, i, ia, id, ieq
    integer :: ierd, ifin, im, imoad, imoda, imode, imodf, iul, jaxe, jpara
    integer :: lcoef, lddad, lfreq, lnom, lnume, lres, ltype, na, nbmoad, nbmoda
    integer :: ladpa, nbmodd, nbmode, nbmodf, nbpar, nbpsmo, nbtrou, nnaxe, nnd
    integer :: lnumm, tmod(1)
!
    real(kind=8) :: r8b, zero, un, coef(3), xnorm
!
    character(len=8) :: k8b, resu, monaxe, formar, chmat, carael
    character(len=8) :: nomnoe, nomcmp, knum, nomdir
    character(len=14) :: nume
    character(len=16) :: concep, acces(3)
    character(len=19) :: chamno, raide
    character(len=24) :: vale, valk, mocb, moatta, moaimp, moauni, mointf, ddlcb
    character(len=24) :: ddlmn, vefreq, ddlac, modele
!
    complex(kind=8) :: c16b
!
    logical :: direct
    logical :: lbid
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
!             123456789012345678901234
    ddlcb= '&&OP0093.DDL_STAT_DEPL'
    mocb=  '&&OP0093.MODE_STAT_DEPL'
    ddlmn= '&&OP0093.DDL_STAT_FORC'
    moatta='&&OP0093.MODE_STAT_FORC'
    moauni='&&OP0093.MODE_STAT_ACCU'
    moaimp='&&OP0093.MODE_ACCE_IMPO'
    ddlac= '&&OP0093.DDL_ACCE_IMPO'
    mointf='&&MOIN93.MODE_INTF_DEPL'
    vefreq='&&MOIN93.FREQ_INTF_DEPL'
!
!---------------------------C
!--                       --C
!-- STOKAGE DES DEFORMEES --C
!--                       --C
!---------------------------C
!
    nbmode = nbmodd + nbmodf + nbmoda + nbmoad + nbmodi
!
    call dismoi('F', 'NB_EQUA', raide, 'MATR_ASSE', neq,&
                k8b, ierd)
    call dismoi('F', 'NOM_MODELE', raide, 'MATR_ASSE', ibid,&
                modele, ierd)
    call dismoi('F', 'CHAM_MATER', raide, 'MATR_ASSE', ibid,&
                chmat, ierd)
    call dismoi('F', 'CARA_ELEM', raide, 'MATR_ASSE', ibid,&
                carael, ierd)
!
    call rscrsd('G', resu, concep, nbmode)
!
    imode = 0
    nive=3
    un=1.d0
    zero=0.d0
    call infniv(ifm, niv)
!--
!-- MODES DE CONTRAINTE
!--
    if (nbmodd .gt. 0) then
        call jeveuo(ddlcb, 'L', lddld)
        call jeveuo(mocb, 'L', lmodd)
!
        do ieq = 1, neq
            if (zi(lddld+ieq-1) .eq. 1) then
                imode = imode + 1
!              --- LE VECTEUR ---
                call rsexch(' ', resu, 'DEPL', imode, chamno,&
                            ierd)
                if (ierd .eq. 100) then
                    call vtcrem(chamno, raide, 'G', 'R')
                else
                    vali = ierd
                    valk = chamno
                    call utmess('F', 'ALGELINE4_38', sk=valk, si=vali)
                endif
                vale(1:19) = chamno
                vale(20:24) = '.VALE'
                call jeveuo(vale, 'E', lvale)
                ind = neq*(imode-1)
                do ie = 0, neq-1
                    zr(lvale+ie) = zr(lmodd+ind+ie)
                end do
                call jelibe(vale)
                call rsnoch(resu, 'DEPL', imode)
!              --- LES PARAMETRES ---
                call rgndas(nume, ieq, nomnoe, nomcmp, k8b,&
                            k8b, k8b)
                call rsadpa(resu, 'E', 1, 'NOEUD_CMP', imode,&
                            0, sjv=lnom, styp=k8b)
                zk16(lnom) = nomnoe//nomcmp
                call rsadpa(resu, 'E', 1, 'NUME_DDL', imode,&
                            0, sjv=lnume, styp=k8b)
                zi(lnume) = ieq
                call rsadpa(resu, 'E', 1, 'NUME_MODE', imode,&
                            0, sjv=lnumm, styp=k8b)
                zi(lnumm) = imode
                call rsadpa(resu, 'E', 1, 'TYPE_DEFO', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'DEPL_IMPO'
                call rsadpa(resu, 'E', 1, 'TYPE_MODE', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'MODE_STA'
                call rsadpa(resu, 'E', 1, 'MODELE', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = modele(1:8)
                call rsadpa(resu, 'E', 1, 'CHAMPMAT', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = chmat
                call rsadpa(resu, 'E', 1, 'CARAELEM', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = carael
                call rsadpa(resu, 'E', 1, 'FREQ', imode,&
                            0, sjv=ltype, styp=k8b)
                zr(ltype) = zero
            endif
        end do
    endif
!--
!-- MODES D'ATTACHE
!--
    if (nbmodf .gt. 0) then
        imodf = 0
        call jeveuo(ddlmn, 'L', lddlf)
        call jeveuo(moatta, 'L', lmodf)
        do ieq = 1, neq
            if (zi(lddlf+ieq-1) .eq. 1) then
                imode = imode + 1
                imodf = imodf + 1
!
!              --- LE VECTEUR ---
                call rsexch(' ', resu, 'DEPL', imode, chamno,&
                            ierd)
                if (ierd .eq. 100) then
                    call vtcrem(chamno, raide, 'G', 'R')
                else
                    vali = ierd
                    valk = chamno
                    call utmess('F', 'ALGELINE4_38', sk=valk, si=vali)
                endif
                vale(1:19) = chamno
                vale(20:24) = '.VALE'
                call jeveuo(vale, 'E', lvale)
                ind = neq*(imodf-1)
                do ie = 0, neq-1
                    zr(lvale+ie) = zr(lmodf+ind+ie)
                end do
                call jelibe(vale)
                call rsnoch(resu, 'DEPL', imode)
!
!              --- LES PARAMETRES ---
                call rgndas(nume, ieq, nomnoe, nomcmp, k8b,&
                            k8b, k8b)
                call rsadpa(resu, 'E', 1, 'NOEUD_CMP', imode,&
                            0, sjv=lnom, styp=k8b)
                zk16(lnom) = nomnoe//nomcmp
                call rsadpa(resu, 'E', 1, 'NUME_DDL', imode,&
                            0, sjv=lnume, styp=k8b)
                zi(lnume) = ieq
                call rsadpa(resu, 'E', 1, 'NUME_MODE', imode,&
                            0, sjv=lnumm, styp=k8b)
                zi(lnumm) = imode
                call rsadpa(resu, 'E', 1, 'TYPE_DEFO', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'FORC_IMPO'
                call rsadpa(resu, 'E', 1, 'TYPE_MODE', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'MODE_STA'
                call rsadpa(resu, 'E', 1, 'MODELE', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = modele(1:8)
                call rsadpa(resu, 'E', 1, 'CHAMPMAT', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = chmat
                call rsadpa(resu, 'E', 1, 'CARAELEM', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = carael
                call rsadpa(resu, 'E', 1, 'FREQ', imode,&
                            0, sjv=ltype, styp=k8b)
                zr(ltype) = zero
            endif
        end do
    endif
!--
!-- MODES A ACCELERATION UNIFORME
!--
    if (nbmoad .gt. 0) then
        imoad = 0
        call jeveuo(moaimp, 'L', lmoad)
        call jeveuo(ddlac, 'L', lddad)
        do ieq = 1, neq
            if (zi(lddad+ieq-1) .eq. 1) then
                imode = imode + 1
                imoad = imoad + 1
!
!              --- LE VECTEUR ---
                call rsexch(' ', resu, 'DEPL', imode, chamno,&
                            ierd)
                if (ierd .eq. 100) then
                    call vtcrem(chamno, raide, 'G', 'R')
                else
                    vali = ierd
                    valk = chamno
                    call utmess('F', 'ALGELINE4_38', sk=valk, si=vali)
                endif
                vale(1:19) = chamno
                vale(20:24) = '.VALE'
                call jeveuo(vale, 'E', lvale)
                ind = neq*(imoad-1)
                do ie = 0, neq-1
                    zr(lvale+ie) = zr(lmoad+ind+ie)
                end do
                call jelibe(vale)
                call rsnoch(resu, 'DEPL', imode)
!
!              --- LES PARAMETRES ---
                call rgndas(nume, ieq, nomnoe, nomcmp, k8b,&
                            k8b, k8b)
                call rsadpa(resu, 'E', 1, 'NOEUD_CMP', imode,&
                            0, sjv=lnom, styp=k8b)
                zk16(lnom) = nomnoe//nomcmp
                call rsadpa(resu, 'E', 1, 'NUME_DDL', imode,&
                            0, sjv=lnume, styp=k8b)
                zi(lnume) = ieq
                call rsadpa(resu, 'E', 1, 'NUME_MODE', imode,&
                            0, sjv=lnumm, styp=k8b)
                zi(lnumm) = imode
                call rsadpa(resu, 'E', 1, 'TYPE_DEFO', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'ACCE_DDL_IMPO'
                call rsadpa(resu, 'E', 1, 'TYPE_MODE', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'MODE_STA'
                call rsadpa(resu, 'E', 1, 'MODELE', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = modele(1:8)
                call rsadpa(resu, 'E', 1, 'CHAMPMAT', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = chmat
                call rsadpa(resu, 'E', 1, 'CARAELEM', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = carael
                call rsadpa(resu, 'E', 1, 'FREQ', imode,&
                            0, sjv=ltype, styp=k8b)
                zr(ltype) = zero
            endif
        end do
    endif
!--
!-- MODES A ACCELERATION IMPOSEE
!--
    if (nbmoda .gt. 0) then
!
        call jeveuo(moauni, 'L', lmoda)
        imoda = 0
        do i = 1, nbpsmo
            direct = .false.
            call getvtx('PSEUDO_MODE', 'AXE', iocc=i, nbval=0, nbret=na)
            if (na .ne. 0) then
                nnaxe = -na
                call wkvect('&&OP0093.AXE', 'V V K8', nnaxe, jaxe)
                call getvtx('PSEUDO_MODE', 'AXE', iocc=i, nbval=nnaxe, vect=zk8( jaxe),&
                            nbret=na)
                ifin = 0
                do ia = 1, nnaxe
                    monaxe = zk8(jaxe+ia-1)
                    if (monaxe(1:1) .eq. 'X') then
                        ifin = ifin + 1
                        acces(ifin) = 'ACCE    X       '
                        coef(1) = un
                        coef(2) = zero
                        coef(3) = zero
                    else if (monaxe(1:1).eq.'Y') then
                        ifin = ifin + 1
                        acces(ifin) = 'ACCE    Y       '
                        coef(1) = zero
                        coef(2) = un
                        coef(3) = zero
                    else if (monaxe(1:1).eq.'Z') then
                        ifin = ifin + 1
                        acces(ifin) = 'ACCE    Z       '
                        coef(1) = zero
                        coef(2) = zero
                        coef(3) = un
                    endif
                end do
                call jedetr('&&OP0093.AXE')
            else
                call getvr8('PSEUDO_MODE', 'DIRECTION', iocc=i, nbval=3, vect=coef,&
                            nbret=na)
                if (na .ne. 0) then
!              --- ON NORME LA DIRECTION ---
                    xnorm = zero
                    do id = 1, 3
                        xnorm = xnorm + coef(id)*coef(id)
                    end do
                    xnorm = un / sqrt(xnorm)
                    do id = 1, 3
                        coef(id) = coef(id) * xnorm
                    end do
                    call getvtx('PSEUDO_MODE', 'NOM_DIR', iocc=i, scal=nomdir, nbret=nnd)
                    direct = .true.
                    ifin = 1
                else
                    goto 70
                endif
            endif
            do im = 1, ifin
                imode = imode + 1
                imoda = imoda + 1
!
!              --- LE VECTEUR ---
                call rsexch(' ', resu, 'DEPL', imode, chamno,&
                            ierd)
                if (ierd .eq. 100) then
                    call vtcrem(chamno, raide, 'G', 'R')
                else
                    vali = ierd
                    valk = chamno
                    call utmess('F', 'ALGELINE4_38', sk=valk, si=vali)
                endif
                vale(1:19) = chamno
                vale(20:24) = '.VALE'
                call jeveuo(vale, 'E', lvale)
                ind = neq*(imoda-1)
                do ie = 0, neq-1
                    zr(lvale+ie) = zr(lmoda+ind+ie)
                end do
                call jelibe(vale)
                call rsnoch(resu, 'DEPL', imode)
!
!              --- LES PARAMETRES ---
                call rsadpa(resu, 'E', 1, 'NOEUD_CMP', imode,&
                            0, sjv=lnom, styp=k8b)
                if (direct) then
                    if (nnd .eq. 0) then
                        call codent(imoda, 'G', knum)
                        nomdir = 'DIR_'//knum(1:4)
                    endif
                    zk16(lnom) = 'ACCE    '//nomdir
                else
                    zk16(lnom) = acces(im)
                    if (acces(im) .eq. 'ACCE    X       ') then
                        coef(1) = un
                        coef(2) = zero
                        coef(3) = zero
                    else if (acces(im).eq.'ACCE    Y       ') then
                        coef(1) = zero
                        coef(2) = un
                        coef(3) = zero
                    else
                        coef(1) = zero
                        coef(2) = zero
                        coef(3) = un
                    endif
                endif
                call rsadpa(resu, 'E', 1, 'COEF_X', imode,&
                            0, sjv=lcoef, styp=k8b)
                zr(lcoef) = coef(1)
                call rsadpa(resu, 'E', 1, 'COEF_Y', imode,&
                            0, sjv=lcoef, styp=k8b)
                zr(lcoef) = coef(2)
                call rsadpa(resu, 'E', 1, 'COEF_Z', imode,&
                            0, sjv=lcoef, styp=k8b)
                zr(lcoef) = coef(3)
                call rsadpa(resu, 'E', 1, 'NUME_MODE', imode,&
                            0, sjv=lnumm, styp=k8b)
                zi(lnumm) = imode
                call rsadpa(resu, 'E', 1, 'TYPE_DEFO', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'ACCE_IMPO'
                call rsadpa(resu, 'E', 1, 'TYPE_MODE', imode,&
                            0, sjv=ltype, styp=k8b)
                zk16(ltype) = 'MODE_STA'
                call rsadpa(resu, 'E', 1, 'MODELE', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = modele(1:8)
                call rsadpa(resu, 'E', 1, 'CHAMPMAT', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = chmat
                call rsadpa(resu, 'E', 1, 'CARAELEM', imode,&
                            0, sjv=ladpa, styp=k8b)
                zk8(ladpa) = carael
            end do
 70         continue
        end do
    endif
!--
!-- MODES D'INTERFACE
!--
    if (nbmodi .gt. 0) then
        call jeveuo(mointf, 'L', lmodd)
        call jeveuo(vefreq, 'L', lfreq)
!
        do ieq = 1, nbmodi
            imode = imode + 1
!
!              --- LE VECTEUR ---
            call rsexch(' ', resu, 'DEPL', imode, chamno,&
                        ierd)
            if (ierd .eq. 100) then
                call vtcrem(chamno, raide, 'G', 'R')
            else
                vali = ierd
                valk = chamno
                call utmess('F', 'ALGELINE4_38', sk=valk, si=vali)
            endif
            vale(1:19) = chamno
            vale(20:24) = '.VALE'
            call jeveuo(vale, 'E', lvale)
            ind = neq*(imode-1)
            do ie = 0, neq-1
                zr(lvale+ie) = zr(lmodd+ind+ie)
            end do
            call jelibe(vale)
            call rsnoch(resu, 'DEPL', imode)
!
!              --- LES PARAMETRES ---
!
            call rsadpa(resu, 'E', 1, 'NOEUD_CMP', imode,&
                        0, sjv=lnom, styp=k8b)
            zk16(lnom) = '  '
            call rsadpa(resu, 'E', 1, 'NUME_DDL', imode,&
                        0, sjv=lnume, styp=k8b)
            zi(lnume) = ieq
            call rsadpa(resu, 'E', 1, 'NUME_MODE', imode,&
                        0, sjv=lnumm, styp=k8b)
            zi(lnumm) = imode
            call rsadpa(resu, 'E', 1, 'TYPE_DEFO', imode,&
                        0, sjv=ltype, styp=k8b)
            zk16(ltype) = 'DEPL_IMPO'
            call rsadpa(resu, 'E', 1, 'TYPE_MODE', imode,&
                        0, sjv=ltype, styp=k8b)
            zk16(ltype) = 'MODE_INT'
            call rsadpa(resu, 'E', 1, 'FREQ', imode,&
                        0, sjv=ltype, styp=k8b)
            zr(ltype) = zr(lfreq+ieq-1)
            call rsadpa(resu, 'E', 1, 'MODELE', imode,&
                        0, sjv=ladpa, styp=k8b)
            zk8(ladpa) = modele(1:8)
            call rsadpa(resu, 'E', 1, 'CHAMPMAT', imode,&
                        0, sjv=ladpa, styp=k8b)
            zk8(ladpa) = chmat
            call rsadpa(resu, 'E', 1, 'CARAELEM', imode,&
                        0, sjv=ladpa, styp=k8b)
            zk8(ladpa) = carael
!
        end do
    endif
!
    call titre()
!
!     --- ECRITURE EVENTUELLE DES VALEURS ET DES VECTEURS PROPRES ---
    formar = '1PE12.5'
    if (niv .gt. 1) then
        call rsorac(resu, 'LONUTI', ibid, r8b, k8b,&
                    c16b, r8b, k8b, tmod, 1,&
                    nbtrou)
        nbmode=tmod(1)
        call wkvect('&&OP0093.ECRITURE.RES', 'V V I', nbmode, lres)
        call rsorac(resu, 'TOUT_ORDRE', ibid, r8b, k8b,&
                    c16b, r8b, k8b, zi(lres), nbmode,&
                    nbtrou)
        call irparb(resu, -1, ' ', '&&OP0093.NOM_PARA', nbpar)
        call jeexin('&&OP0093.NOM_PARA', iret)
        if (iret .gt. 0) then
            call jeveuo('&&OP0093.NOM_PARA', 'L', jpara)
        else
            jpara = 1
            nbpar = 0
        endif
        r8b = 0.d0
        ibid = 0
        k8b = ' '
        iul = iunifi( 'MESSAGE' )
        call irecri(resu, 'RESULTAT', iul, k8b, lbid,&
                    ibid, k8b, ' ', nbpar, zk16(jpara),&
                    nbmode, zi(lres), .true., k8b, ibid,&
                    'T', k8b, .false., ibid, [0],&
                    ibid, [0], ibid, k8b, .false.,&
                    r8b, .false., r8b, .false., .false.,&
                    formar, nive, versio)
    endif
!
!
!
!     ------------------------------------------------------------------
!
    call jedema()
end subroutine
