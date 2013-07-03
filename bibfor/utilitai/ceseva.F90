subroutine ceseva(cesf, npara, lpara, cesr)
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
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    implicit none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cestas.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: npara
    character(len=*) :: cesf, lpara(npara), cesr
! ---------------------------------------------------------------------
! BUT: EVALUER LE CHAM_ELEM_S DE FONCTIONS CESF EN UTILISANT
!      LES PARAMETRES TROUVES DANS LES CHAM_ELEM_S LPARA
! ---------------------------------------------------------------------
! ARGUMENTS:
! CESF  IN/JXIN  K19 : SD CHAM_ELEM_S A EVALUER
! NPARA IN       I   : NOMBRE DE CHAM_ELEM_S PARAMETRES (LPARA)
! LPARA IN/JXIN  V(K19) : LISTE DES CHAM_ELEM_S PARAMETRES
! CESR  IN/JXOUT K19  : SD CHAM_ELEM RESULTAT DE L'EVALUATION
!
! REMARQUES :
!  EN CHAQUE POINT DE DISCRETISATION DE CESF, ON FERA "CALL FOINTE"
!  POUR EVALUER LES FONCTIONS AFFECTEES A CE POINT.
!  ON PASSERA EN PARAMETRES DES FONCTIONS, LES VALEURS DES CHAMPS
!  DE LPARA AVEC COMME NOM DE PARAMETRE LE NOM DE LA CMP
!  ON NE TRAITE QUE LES FONCTIONS REELLES F : R * R(* R,...) -> R
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: jfd, jfc, jfv, jfl, jfk
    integer :: jpd, jpc, jpv, jpl, jpk
    integer :: jrd, jrc, jrv, jrl, jrk
    integer :: nbma, ib, k, ima, ncmp, nbpu, ier, nbpumx, ibid
    integer :: ncmp2, ipara, jad1, ncmpmx, nspmx, nptmx
    integer :: k2, iadf, iadr, iadp, nbpt, nbsp, ipt, isp, jnompu, jvalpu
    character(len=8) :: ma, nomgdf, nomgdr, fo
    character(len=8) :: ma2, nomgd2, typces
    character(len=3) :: tsca
    character(len=19) :: f, p, r
    character(len=24) :: valk
    real(kind=8) :: x
!     ------------------------------------------------------------------
    call jemarq()
!
!     -- 2 VECTEURS POUR STOCKER LE NOM ET LES VALEURS DES PARAMETRES
!        DES FONCTIONS :
    nbpumx=10
    call wkvect('&&CESEVA.NOMPU', 'V V K8', nbpumx, jnompu)
    call wkvect('&&CESEVA.VALPU', 'V V R', nbpumx, jvalpu)
!
!     1- RECUPERATIONS D'INFOS DANS LE CHAMP DE FONCTIONS :
!     ------------------------------------------------------------
    f = cesf
    call jeveuo(f//'.CESK', 'L', jfk)
    call jeveuo(f//'.CESD', 'L', jfd)
    call jeveuo(f//'.CESC', 'L', jfc)
    call jeveuo(f//'.CESV', 'L', jfv)
    call jeveuo(f//'.CESL', 'L', jfl)
!
    ma = zk8(jfk-1+1)
    nomgdf = zk8(jfk-1+2)
    typces = zk8(jfk-1+3)
    nbma = zi(jfd-1+1)
    ncmp = zi(jfd-1+2)
    nptmx = zi(jfd-1+3)
    nspmx = zi(jfd-1+4)
    ncmpmx = zi(jfd-1+5)
!
    call dismoi('F', 'TYPE_SCA', nomgdf, 'GRANDEUR', ib,&
                tsca, ib)
    if (tsca .ne. 'K8') call u2mess('F', 'UTILITAI_16')
!
!     2- ALLOCATION DU CHAM_ELEM_S RESULTAT ET RECUPERATION
!        DES ADRESSES DE SES OBJETS   :
!     ------------------------------------------------------------
    r = cesr
    nomgdr = nomgdf(1:4)//'_R'
    call cescre('V', r, typces, ma, nomgdr,&
                ncmp, zk8(jfc), -nptmx, -nspmx, -ncmpmx)
    call jeveuo(r//'.CESK', 'L', jrk)
    call jeveuo(r//'.CESD', 'L', jrd)
    call jeveuo(r//'.CESC', 'L', jrc)
    call jeveuo(r//'.CESV', 'E', jrv)
    call jeveuo(r//'.CESL', 'E', jrl)
!
!
!     3- ON MET EN MEMOIRE LES OBJETS UTILES DES CHAMPS PARAMETRES :
!     --------------------------------------------------------------
    call wkvect('&&CESEVA.JAD1', 'V V I', 4*npara, jad1)
    do 10,ipara = 1,npara
    p = lpara(ipara)
    call jeveuo(p//'.CESK', 'L', jpk)
    call jeveuo(p//'.CESD', 'L', jpd)
    call jeveuo(p//'.CESC', 'L', jpc)
    call jeveuo(p//'.CESV', 'L', jpv)
    call jeveuo(p//'.CESL', 'L', jpl)
    ma2 = zk8(jpk-1+1)
    nomgd2 = zk8(jpk-1+2)
!
    call dismoi('F', 'TYPE_SCA', nomgd2, 'GRANDEUR', ib,&
                tsca, ib)
    if (tsca .ne. 'R') call u2mess('F', 'UTILITAI_17')
    if (ma2 .ne. ma) call u2mess('F', 'UTILITAI_18')
    zi(jad1-1+4* (ipara-1)+1) = jpc
    zi(jad1-1+4* (ipara-1)+2) = jpd
    zi(jad1-1+4* (ipara-1)+3) = jpl
    zi(jad1-1+4* (ipara-1)+4) = jpv
    10 end do
!
!     4- EVALUATION DES FONCTIONS :
!     ---------------------------------------
!     ON BOUCLE D'ABORD SUR LES CMPS POUR AVOIR PLUS DE CHANCES
!     DE FAIRE PLUSIEURS FOINTE SUCCESSIFS AVEC LA MEME FONCTION.
!
    do 70,k = 1,ncmp
    do 60,ima = 1,nbma
    nbpt = zi(jfd-1+5+4* (ima-1)+1)
    nbsp = zi(jfd-1+5+4* (ima-1)+2)
    do 50,ipt = 1,nbpt
    do 40,isp = 1,nbsp
    call cesexi('C', jfd, jfl, ima, ipt,&
                isp, k, iadf)
    if (iadf .le. 0) goto 40
!
    fo = zk8(jfv-1+iadf)
!
    call cesexi('C', jrd, jrl, ima, ipt,&
                isp, k, iadr)
    call assert(iadr.lt.0)
    zl(jrl-1-iadr) = .true.
!
    if (fo .eq. ' ') goto 40
!
!           4.1 FABRICATION DE LA LISTE DES PARAMETRES POUR FOINTE:
!           -------------------------------------------------------
    nbpu = 0
    do 30,ipara = 1,npara
    jpc = zi(jad1-1+4* (ipara-1)+1)
    jpd = zi(jad1-1+4* (ipara-1)+2)
    jpl = zi(jad1-1+4* (ipara-1)+3)
    jpv = zi(jad1-1+4* (ipara-1)+4)
    ncmp2 = zi(jpd-1+2)
    do 20,k2 = 1,ncmp2
    call cesexi('C', jpd, jpl, ima, ipt,&
                isp, k2, iadp)
    if (iadp .le. 0) goto 20
!
    nbpu = nbpu + 1
    if (nbpu .gt. nbpumx) then
!                    -- ON AGRANDIT .NOMPU ET .VALPU :
        nbpumx=2*nbpumx
        call juveca('&&CESEVA.NOMPU', nbpumx)
        call juveca('&&CESEVA.VALPU', nbpumx)
        call jeveuo('&&CESEVA.NOMPU', 'E', jnompu)
        call jeveuo('&&CESEVA.VALPU', 'E', jvalpu)
    endif
!
!                 -- ON VERIFIE QU'UN MEME PARAMETRE N'EST PAS AJOUTE
!                    PLUSIEURS FOIS:
    ibid=indik8(zk8(jnompu),zk8(jpc-1+k2),1,&
                            nbpu-1)
    if (ibid .gt. 0) call u2mesk('F', 'CALCULEL2_78', 1, zk8(jpc-1+k2))
!
    zk8(jnompu-1+nbpu) = zk8(jpc-1+k2)
    zr(jvalpu-1+nbpu) = zr(jpv-1+iadp)
20  continue
30  continue
!
!
!           4.2 APPEL A FOINTE :
!           --------------------
    call fointe('E', fo, nbpu, zk8(jnompu), zr(jvalpu),&
                x, ier)
    if (ier .ne. 0) then
        call u2mesk('F+', 'FONCT0_9', 1, fo)
        call jenuno(jexnum(ma//'.NOMMAI', ima), valk)
        call u2mesk('F', 'FONCT0_10', 1, valk)
    endif
!
!           4.3 STOCKAGE DU RESULTAT :
!           --------------------------
    zr(jrv-1-iadr) = x
!
40  continue
50  continue
60  continue
    70 end do
!
    call cestas(cesr)
!
!     5- MENAGE :
!     ---------------------------------------
    call jedetr('&&CESEVA.JAD1')
    call jedetr('&&CESEVA.NOMPU')
    call jedetr('&&CESEVA.VALPU')
!
    call jedema()
end subroutine
