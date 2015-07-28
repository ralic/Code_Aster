subroutine xrecff(fiss, typfis, chfond, basfon, fonoeu, lnoff, conf)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/dismoi.h"
#include "asterfort/jelira.h"
    integer :: lnoff
    character(len=8) :: fiss, typfis, conf
    character(len=24) :: chfond, basfon, fonoeu
!
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!       RECUPERATION DE LA LISTE DES POINTS DU FOND DE FISSURE
!       SUR LEQUEL ON VA EFFECTUER LE CALCUL + BASE LOCALE EN FOND
!       DE FISSURE
!
!  IN  : FISS   : SD_FISS_XFEM OU SD_FOND_FISS
!  IN  : TYPFIS : TYPE D'OBJET POUR DECRIRE LE FOND DE FISSURE
!                 'FONDFISS' OU 'FISSURE' OU 'THETA'
!  OUT : CHFOND : FOND DE FISSURE SUR LEQUEL ON FERA LE POST TRAITEMENT
!  OUT : BASFON : BASE LOCALE RELATIVE A CHFOND
!  OUT : FONOEU : NOEUD du FOND DE FISSURE (FEM)
!  OUT : LNOFF  : NOMBRE DE POINTS DU FOND CHFOND
!  OUT : CONF   : CONFIGURATION DE LA FISSURE (FEM)
!
!     ------------------------------------------------------------------
!
    integer ::   numfon, ibid, idepfi, iarrfi, ifon, ibas, inoeu
!
    integer :: i, j, k, nfonu, jfonu,  jbasu, jnoeu
    real(kind=8) :: smax, s, s1, s2, xyz1, xyz2
    character(len=24) :: fontmp, bastmp, noeutmp, valk
    integer, pointer :: fondmult(:) => null()
    real(kind=8), pointer :: fondfiss(:) => null()
    real(kind=8), pointer :: basefond(:) => null()
    character(len=8), pointer :: fonoeud(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()

!     LISTE DES NOEUDS DU FOND DE FISSURE EN FEM
    if (typfis.eq.'FONDFISS') then
        call jeveuo(fiss//'.FOND.NOEU', 'L', vk8=fonoeud)
    endif
!
!     LISTE DES POINTS DES FONDS DE FISSURES
    call jeveuo(fiss//'.FONDFISS', 'L', vr=fondfiss)
!
!     LISTE DES FONDS MULTIPLES EN XFEM
    if (typfis.eq.'FISSURE') then
        call jeveuo(fiss//'.FONDMULT', 'L', vi=fondmult)
    endif
!
!     BASE LOCALE EN FOND DE FISSURE
    if (typfis.eq.'FONDFISS') then
!       CET OBJET N'EXISTE QUE SI CONFIG_INIT='COLLEE'
        call dismoi('CONFIG_INIT', fiss, 'FOND_FISS', repk=conf)
        if (conf .eq. 'COLLEE') then
            call jeveuo(fiss//'.BASEFOND', 'L', vr=basefond)
        endif
    else
        call jeveuo(fiss//'.BASEFOND', 'L', vr=basefond)
    endif
!
!     ------------------------------------------------------------------
!     TRAITEMENT DU MOT-CLE NUME_FOND :
!     RESTRICTION DU FOND ET DE LA BASE AU NUMERO DU FOND DEMANDE
!     ------------------------------------------------------------------
!
!     NUMERO DU FOND A TRAITER
    if (typfis.eq.'FISSURE') then
        call getvis('THETA', 'NUME_FOND', iocc=1, scal=numfon, nbret=ibid)
    else
        numfon=1
    endif
!
!     DETERMINATION DU NOMBRE DE NOEUDS EN FOND DE FISSURE
    if (typfis.eq.'FISSURE') then
        idepfi=fondmult(2*(numfon-1)+1)
        iarrfi=fondmult(2*(numfon-1)+2)
        lnoff=iarrfi-idepfi+1
    else
        idepfi=1
        call jelira(fiss//'.FOND.NOEU', 'LONMAX', lnoff)
    endif
!
!     CREATION DE NOEUDS TEMPORAIRES
    if (typfis.eq.'FONDFISS') then
        noeutmp = '&&XREFF.FONNOEU_TEMP'
        call wkvect(noeutmp, 'V V K8', lnoff, inoeu)
        do 14 i = 1, lnoff
            zk8(inoeu-1+i)=fonoeud(i)
14      continue
    endif
!
!     CREATION D'UN FOND TEMPORAIRE RESTREINT AU NUMFON
    fontmp = '&&XREFF.FONFIS_TEMP'
    call wkvect(fontmp, 'V V R', lnoff*4, ifon)
    do 15 i = 1, lnoff
        do 16 j = 1, 4
            zr(ifon-1+4*(i-1)+j)=fondfiss(4*(i+(idepfi-1)-1)+j)
16      continue
15  continue
!
!     CREATION D'UNE BASE TEMPORAIRE RESTREINTE AU NUMFON
    if ((typfis.eq.'FISSURE').or.(conf .eq. 'COLLEE')) then
        bastmp = '&&XREFF.BASFON_TEMP'
        call wkvect(bastmp, 'V V R', lnoff*6, ibas)
        do 17 i = 1, lnoff
            do 18 j = 1, 6
                zr(ibas-1+6*(i-1)+j)=basefond(6*(i+(idepfi-1)-1)+j)
18          continue
17      continue
    endif
!
!     ------------------------------------------------------------------
!     TRAITEMENT DU MOT-CLE NB_POINT_FOND :
!     CREATION DU NOUVEAU FOND ET DE LA NOUVELLE BASE
!     ------------------------------------------------------------------
!
!     DOIT-ON PRENDRE UNE REPARTITION UNIFORME ?
    call getvis('THETA', 'NB_POINT_FOND', iocc=1, scal=nfonu, nbret=ibid)
    if (ibid .eq. 0) nfonu = 0
!
    if (nfonu .gt. 0) then
!
!       SI OUI : MODIFICATION DE LA LISTE DES POINTS DU FOND
!                ET DE LA BASE
!
        ASSERT(nfonu.ge.2)
!
!       CREATION DU FOND MODIFIE
        call wkvect(chfond, 'V V R', 4*nfonu, jfonu)
!
!       CREATION DE LA BASE MODIFIEE
        if ((typfis.eq.'FISSURE').or.(conf .eq. 'COLLEE')) then
            call wkvect(basfon, 'V V R', 6*nfonu, jbasu)
        endif
!
!       CREATION DES NOEUDS MODIFIES
        if (typfis.eq.'FONDFISS') then
            call wkvect(fonoeu, 'V V K8', nfonu, jnoeu)
        endif
!
!       1ER ET DERNIER POINTS
        do 100 j = 1, 4
            zr(jfonu-1+4*(1-1)+j)=zr(ifon-1+4*(1-1)+j)
            zr(jfonu-1+4*(nfonu-1)+j)=zr(ifon-1+4*(lnoff-1)+j)
100     continue
!
        if ((typfis.eq.'FISSURE').or.(conf .eq. 'COLLEE')) then
            do 101 j = 1, 6
                zr(jbasu-1+6*(1-1)+j)=zr(ibas-1+6*(1-1)+j)
                zr(jbasu-1+6*(nfonu-1)+j)=zr(ibas-1+6*(lnoff-1)+j)
101         continue
        endif
!
        if (typfis.eq.'FONDFISS') then
            zk8(jnoeu)='XXXX'
            zk8(jnoeu+(nfonu-1))='XXXX'
        endif       
!
!       NOUVEAUX POINTS
        smax = zr(ifon-1+4*(lnoff-1)+4)
        do 102 i = 2, nfonu-1
            s = (i-1)*smax/(nfonu-1)
            do 103 k = 1, lnoff
                if (zr(ifon-1+4*(k-1)+4) .gt. s) goto 110
103          continue
110          continue
!         ON INTERPOLE LES COORD ENTRE CELLES DU SEGMENT [K-1,K]
            s1 = zr(ifon-1+4*(k-1-1)+4)
            s2 = zr(ifon-1+4*( k-1)+4)
            do 111 j = 1, 3
                xyz1 = zr(ifon-1+4*(k-1-1)+j)
                xyz2 = zr(ifon-1+4*( k-1)+j)
                zr(jfonu-1+4*(i-1)+j) = xyz1 + (xyz2-xyz1)*(s-s1)/(s2- s1)
111          continue
            if ((typfis.eq.'FISSURE').or.(conf .eq. 'COLLEE')) then
                do 112 j = 1, 6
                    xyz1 = zr(ibas-1+6*(k-1-1)+j)
                    xyz2 = zr(ibas-1+6*( k-1)+j)
                    zr(jbasu-1+6*(i-1)+j) = xyz1 + (xyz2-xyz1)*(s-s1)/(s2- s1)
112             continue
            endif
!           
            zr(jfonu-1+4*(i-1)+4) = s
102         continue
!
!       CREATION DES NOEUDS MODIFIES
        if (typfis.eq.'FONDFISS') then
            valk='XXXX'
!
!           FONOEU : VALEURS MISES A XXXX
            do 104 i = 1, nfonu
                zk8(jnoeu-1+i) = valk
104         continue
        endif
!
!       ON ECRASE LNOFF
        lnoff = nfonu
!
    else
!
!       SI NON : ON RECOPIE TELLE QUELLE LA LISTE DES POINTS DU FOND
        call jedupo(fontmp, 'V', chfond, .false._1)
!
!       ET ON RECOPIE TELLE QUELLE LA BASE
        call jedupo(bastmp, 'V', basfon, .false._1)
!
!       ET ON RECOPIE TELS QUELS LES NOEUDS
        if (typfis.eq.'FONDFISS') then
            call jedupo(noeutmp, 'V', fonoeu, .false._1)
        endif
    endif
!
!     MENAGE
    call jedetr(fontmp)
    call jedetr(bastmp)
    call jedetr(noeutmp)    
!
    call jedema()
end subroutine
