subroutine jevech(nmparz, louez, itab)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/chloet.h"
#include "asterfort/contex.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: nmparz, louez
    character(len=8) :: nompar, nommai
    character(len=1) :: loue
    integer :: itab
!     -----------------------------------------------------------------
!     ENTREES:
!     NOMPAR  : NOM DU PARAMETRE DE L'OPTION
!     LOUEZ   : 'L' OU 'E'  ( LECTURE/ECRITURE )
!
!     SORTIES:
!     ITAB     : ADRESSE DU CHAMP LOCAL CORRESPONDANT A NOMPAR
!     -----------------------------------------------------------------
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
!
    integer :: iachlo, iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa
    integer :: ilchlo, k, kk, debugr
    integer :: iel, ilmloc, ilopmo, iaoptt
    integer :: ilopno, iparg, lgco, npario, lgcata
    integer :: nparin, iachii, iachik, iachix, iachoi, iachok, jceld, adiel
    integer :: debgr2, lonchl, decael, iadzi, iazk24, jrsvi, jcrsvi, i1
    integer :: opt, iaopd2, iaoplo, iapara, ipara, npari2
!
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii08/iel
    common /caii04/iachii,iachik,iachix
    common /caii07/iachoi,iachok
    integer :: caindz(512), capoiz, ich
    common /caii12/caindz,capoiz
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
    logical :: etendu
    character(len=24) :: valk(5)
!
!
! DEB -----------------------------------------------------------------
    nompar = nmparz
    loue = louez
!
    ASSERT(loue.eq.'L' .or. loue.eq.'E')
!
!     -- RECHERCHE DE LA CHAINE NOMPAR AVEC MEMOIRE SUR TOUT 'CALCUL'
    capoiz = capoiz + 1
    if (capoiz .gt. 512) then
        iparg = indik8(zk8(iaoppa),nompar,1,npario)
    else
        if (zk8(iaoppa-1+caindz(capoiz)) .eq. nompar) then
            iparg = caindz(capoiz)
        else
            iparg = indik8(zk8(iaoppa),nompar,1,npario)
            caindz(capoiz) = iparg
        endif
    endif
!
!
    if (iparg .eq. 0) then
        valk(1) = nompar
        valk(2) = option
        call utmess('E', 'CALCULEL2_69', nk=2, valk=valk)
        call contex(option, ' ')
    endif
!
! --- ON VERIFIE QUE LES PARAMETRE IN SONT EN LECTURE
!     ET QUE LES PARAMETRES OUT SONT EN ECRITURE
    if (iparg .gt. nparin .and. loue .eq. 'L') then
        write(6,*)'PARAMETRE OUT EN LECTURE : ',nompar
        ASSERT(.false.)
    else if (iparg.le.nparin .and. loue.eq.'E') then
        write(6,*)'PARAMETRE IN EN ECRITURE : ',nompar
        ASSERT(.false.)
    endif
!
    iachlo=zi(iawloc-1+3*(iparg-1)+1)
    ilchlo=zi(iawloc-1+3*(iparg-1)+2)
    lgcata=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)
    debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
!
!     -- CALCUL DE JRSVI ET JCRSVI :
    jrsvi=0
    if (evfini .eq. 1) then
        ich=zi(iawloc-1+3*(iparg-1)+3)
        if (iparg .gt. nparin .and. ich .gt. 0) then
            if (zk8(iachok-1+2*(ich-1)+1) .eq. 'RESL') then
                jrsvi=zi(iachoi-1+3*(ich-1)+2)
                jcrsvi=zi(iachoi-1+3*(ich-1)+3)
            endif
        endif
    endif
!
    if (lgcata .eq. -1) then
        valk(1) = nompar
        valk(2) = option
        valk(3) = nomte
        call utmess('E', 'CALCULEL2_70', nk=3, valk=valk)
        call contex(option, nompar)
    endif
!
!
    if (iachlo .eq. -1) then
!        ON AJOUTE CELA POUR EMETTRE UN MESSAGE PLUS CLAIR DANS
!        LE CAS OU IL MANQUE UN CHAMP LIE A UN PARAMETRE
        call jenonu(jexnom('&CATA.OP.NOMOPT', option), opt)
        call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', iaopd2)
        call jeveuo(jexnum('&CATA.OP.LOCALIS', opt), 'L', iaoplo)
        call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', iapara)
        npari2 = zi(iaopd2-1+2)
        do 20 ipara = 1, npari2
            if (zk8(iapara+ipara-1) .eq. nompar) goto 30
20      continue
        goto 40
30      continue
        valk(1) = option
!        ON PEUT TROUVER D'OU VIENT LE PROBLEME DANS 3 CAS
        if (zk24(iaoplo+3*ipara-3) .eq. 'CARA') then
            call utmess('E', 'CALCULEL2_53', sk=valk(1))
        else if (zk24(iaoplo+3*ipara-3).eq.'CHMA') then
            call utmess('E', 'CALCULEL2_54', sk=valk(1))
        else if (zk24(iaoplo+3*ipara-3).eq.'MODL') then
            call utmess('E', 'CALCULEL2_55', sk=valk(1))
        endif
40      continue
        valk(1) = nompar
        valk(2) = option
        valk(3) = nomte
        call utmess('E', 'CALCULEL2_71', nk=3, valk=valk)
        call contex(option, nompar)
!
    endif
    ASSERT(iachlo.ne.-2)
!
!
!     -- CALCUL DE ITAB,LONCHL,DECAEL :
!     ---------------------------------
    call chloet(iparg, etendu, jceld)
    if (etendu) then
        adiel = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+4)
        debgr2 = zi(jceld-1+zi(jceld-1+4+igr)+8)
        ASSERT(lgcata.eq.zi(jceld-1+zi(jceld-1+4+igr)+3))
        decael = (adiel-debgr2)
        lonchl = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+3)
    else
        if (jrsvi .eq. 0) then
            decael = (iel-1)*lgcata
            lonchl = lgcata
        else
            i1 = zi(jcrsvi-1+igr)
            decael = zi(jrsvi-1+i1-1+iel)
            lonchl = zi(jrsvi-1+i1-1+iel+1) - decael
            decael = decael -1
        endif
    endif
    itab = iachlo+debugr-1+decael
!
!
!     -- POUR LES CHAMPS "IN" ON VERIFIE QUE L'EXTRACTION EST
!        COMPLETE SUR L'ELEMENT:
!     ----------------------------------------------------------
    if (ilchlo .ne. -1) then
        do 10,k = 1,lonchl
        if (.not.zl(ilchlo+debugr-1+decael-1+k)) then
            call tecael(iadzi, iazk24)
            nommai=zk24(iazk24-1+3)(1:8)
            valk(1) = nompar
            valk(2) = option
            valk(3) = nomte
            valk(4) = nommai
!
!           -- POUR CERTAINS PARAMETRES "COURANTS" ON EMET
!              UN MESSAGE PLUS CLAIR :
            if (nompar .eq. 'PMATERC') then
                call utmess('F', 'CALCULEL2_74', nk=4, valk=valk)
            else if (nompar.eq.'PCACOQU') then
                call utmess('F', 'CALCULEL2_75', nk=4, valk=valk)
            else if (nompar.eq.'PCAGNPO') then
                call utmess('F', 'CALCULEL2_76', nk=4, valk=valk)
            else if (nompar.eq.'PCAORIE') then
                call utmess('F', 'CALCULEL2_77', nk=4, valk=valk)
!
            else
                call utmess('E', 'CALCULEL2_73', nk=4, valk=valk)
!
                write (6,*) 'ERREUR JEVECH ZL :',nompar, (zl(&
                    ilchlo+debugr-1+decael-1+kk),kk=1,lonchl)
                write (6,*) 'MAILLE: ',zk24(iazk24-1+3)
                call contex(option, nompar)
            endif
        endif
10      continue
    endif
!
end subroutine
