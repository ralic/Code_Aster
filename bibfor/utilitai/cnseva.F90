subroutine cnseva(cnsf, npara, lpara, cnsr)
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
#include "asterc/indik8.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: npara
    character(len=*) :: cnsf, lpara(npara), cnsr
! ---------------------------------------------------------------------
! BUT: EVALUER LE CHAM_NO_S DE FONCTIONS CNSF EN UTILISANT
!      LES PARAMETRES TROUVES DANS LES CHAM_NO_S LPARA
! ---------------------------------------------------------------------
! ARGUMENTS:
! CNSF  IN/JXIN  K19 : SD CHAM_NO_S A EVALUER
! NPARA IN       I   : NOMBRE DE CHAM_NO_S PARAMETRES (LPARA)
! LPARA IN/JXIN  V(K19) : LISTE DES CHAM_NO_S PARAMETRES
! CNSR  IN/JXOUT K19  : SD CHAM_NO RESULTAT DE L'EVALUATION
!
! REMARQUES :
!  EN CHAQUE POINT DE DISCRETISATION DE CNSF, ON FERA "CALL FOINTE"
!  POUR EVALUER LES FONCTIONS AFFECTEES A CE POINT.
!  ON PASSERA EN PARAMETRES DES FONCTIONS, LES VALEURS DES CHAMPS
!  DE LPARA AVEC COMME NOM DE PARAMETRE LE NOM DE LA CMP
!
!  ON NE TRAITE QUE LES FONCTIONS REELLES F : R * R(* R,...) -> R
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jfd, jfc, jfv, jfl, jfk
    integer :: jpd, jpc, jpv, jpl, jpk
    integer :: jrd, jrc, jrv, jrl, jrk
    integer :: nbno, ib, k, ino, ncmp, nbpu, ier, nbpumx
    integer :: k2, ncmp2, ipara, jad1, ibid
    parameter (nbpumx=50)
    character(len=8) :: ma, nomgdf, nomgdr, fo, nompu(nbpumx)
    character(len=8) :: ma2, nomgd2
    character(len=3) :: tsca
    character(len=19) :: f, p, r
    character(len=24) :: valk
    real(kind=8) :: x, valpu(nbpumx)
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     1- RECUPERATIONS D'INFOS DANS LE CHAMP DE FONCTIONS :
!     ------------------------------------------------------------
    f = cnsf
    call jeveuo(f//'.CNSK', 'L', jfk)
    call jeveuo(f//'.CNSD', 'L', jfd)
    call jeveuo(f//'.CNSC', 'L', jfc)
    call jeveuo(f//'.CNSV', 'L', jfv)
    call jeveuo(f//'.CNSL', 'L', jfl)
!
    ma = zk8(jfk-1+1)
    nomgdf = zk8(jfk-1+2)
    nbno = zi(jfd-1+1)
    ncmp = zi(jfd-1+2)
!
    call dismoi('F', 'TYPE_SCA', nomgdf, 'GRANDEUR', ib,&
                tsca, ib)
    if (tsca .ne. 'K8') then
        call utmess('F', 'UTILITAI_16')
    endif
!
!
!     2- ALLOCATION DU CHAM_NO_S RESULTAT ET RECUPERATION
!        DES ADRESSES DE SES OBJETS   :
!     ------------------------------------------------------------
    r = cnsr
    nomgdr = nomgdf(1:4)//'_R'
    call cnscre(ma, nomgdr, ncmp, zk8(jfc), 'V',&
                r)
    call jeveuo(r//'.CNSK', 'L', jrk)
    call jeveuo(r//'.CNSD', 'L', jrd)
    call jeveuo(r//'.CNSC', 'L', jrc)
    call jeveuo(r//'.CNSV', 'E', jrv)
    call jeveuo(r//'.CNSL', 'E', jrl)
!
!
!     3- ON MET EN MEMOIRE LES OBJETS UTILES DES CHAMPS PARAMETRES :
!     --------------------------------------------------------------
    call wkvect('&&CNSEVA.JAD1', 'V V I', 4*npara, jad1)
    do 10,ipara = 1,npara
    p = lpara(ipara)
    call jeveuo(p//'.CNSK', 'L', jpk)
    call jeveuo(p//'.CNSD', 'L', jpd)
    call jeveuo(p//'.CNSC', 'L', jpc)
    call jeveuo(p//'.CNSV', 'L', jpv)
    call jeveuo(p//'.CNSL', 'L', jpl)
    ma2 = zk8(jpk-1+1)
    nomgd2 = zk8(jpk-1+2)
!
    call dismoi('F', 'TYPE_SCA', nomgd2, 'GRANDEUR', ib,&
                tsca, ib)
    if (tsca .ne. 'R') then
        call utmess('F', 'UTILITAI_17')
    endif
    if (ma2 .ne. ma) then
        call utmess('F', 'UTILITAI_18')
    endif
    zi(jad1-1+4* (ipara-1)+1) = jpc
    zi(jad1-1+4* (ipara-1)+2) = jpd
    zi(jad1-1+4* (ipara-1)+3) = jpl
    zi(jad1-1+4* (ipara-1)+4) = jpv
    10 end do
!
!
!
!     4- EVALUATION DES FONCTIONS :
!     ---------------------------------------
!     ON BOUCLE D'ABORD SUR LES CMPS POUR AVOIR PLUS DE CHANCES
!     DE FAIRE PLUSIEURS FOINTE SUCCESSIFS AVEC LA MEME FONCTION.
!
    do 50,k = 1,ncmp
    do 40,ino = 1,nbno
    if (zl(jfl-1+ (ino-1)*ncmp+k)) then
        zl(jrl-1+ (ino-1)*ncmp+k) = .true.
        fo = zk8(jfv-1+ (ino-1)*ncmp+k)
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
        if (zl(jpl-1+ (ino-1)*ncmp2+k2)) then
            nbpu = nbpu + 1
            if (nbpu .gt. nbpumx) then
                call utmess('F', 'CALCULEL2_66')
            endif
!
!                 -- ON VERIFIE QU'UN MEME PARAMETRE N'EST PAS AJOUTE
!                    PLUSIEURS FOIS:
            ibid=indik8(nompu,zk8(jpc-1+k2),1,nbpu-1)
            if (ibid .gt. 0) then
                call utmess('F', 'CALCULEL2_78', sk=zk8(jpc-1+k2))
            endif
!
            nompu(nbpu) = zk8(jpc-1+k2)
            valpu(nbpu) = zr(jpv-1+ (ino-1)*ncmp2+k2)
        endif
20      continue
30      continue
!
!
!           4.2 APPEL A FOINTE :
!           --------------------
        call fointe('E', fo, nbpu, nompu, valpu,&
                    x, ier)
        if (ier .ne. 0) then
            call utmess('F+', 'FONCT0_9', sk=fo)
            call jenuno(jexnum(ma//'.NOMNOE', ino), valk)
            call utmess('F', 'FONCT0_53', sk=valk)
        endif
!
!           4.3 STOCKAGE DU RESULTAT :
!           --------------------------
        zr(jrv-1+ (ino-1)*ncmp+k) = x
!
    endif
40  continue
    50 end do
!
!
!     5- MENAGE :
!     ---------------------------------------
    call jedetr('&&CNSEVA.JAD1')
!
    call jedema()
end subroutine
