subroutine caundf(code, opt, te)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
!
    include 'asterc/iisnan.h'
    include 'asterc/indik8.h'
    include 'asterc/isnnem.h'
    include 'asterc/r8nnem.h'
    include 'asterfort/assert.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbpara.h'
    include 'asterfort/nopara.h'
    include 'asterfort/u2mesk.h'
    integer :: opt, te
    character(len=5) :: code
! ----------------------------------------------------------------------
!     ENTREES:
!      CODE :  / 'ECRIT' : ON ECRIT UNE VALEUR UNDEF AU BOUT DES CHLOC
!              / 'VERIF' : ON VERIFIE LA VALEUR UNDEF AU BOUT DES CHLOC
!      OPT : OPTION
!      TE  : TYPE_ELEMENT
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii04/iachii,iachik,iachix
    common /caii07/iachoi,iachok
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
    integer :: innem
    integer :: np, ipar
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds, iaoppa
    integer :: npario, nparin, iamloc, ilmloc, iadsgd
    integer :: iachii, iachik, iachix, iachoi
    integer :: iachok, iparg, lggrel, iachlo
    character(len=3) :: typsca
    character(len=8) :: nompar
    logical :: ecras, arret
    character(len=16) :: nomte, nomopt
    integer :: ich, debugr, lgcata
    real(kind=8) :: rnnem
    character(len=8) :: knnem
    character(len=24) :: valk(3)
!
! DEB-------------------------------------------------------------------
!
    innem = isnnem()
    rnnem = r8nnem()
    knnem='????????'
!
    call assert((code.eq.'ECRIT').or.(code.eq.'VERIF'))
!
!
    if (code .eq. 'ECRIT') then
!     ------------------------------------------------
!
!        -- CHAMPS "IN" ET "OUT" :
        do 10 iparg = 1, npario
            lgcata=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)
            if (lgcata .le. 0) goto 10
            iachlo=zi(iawloc-1+3*(iparg-1)+1)
            if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) goto 10
!
            typsca = zk8(iawtyp-1+iparg)
            lggrel=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+4)
            debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
!
            if (typsca .eq. 'R') then
                zr(iachlo-1+debugr-1+lggrel+1) = rnnem
            else if (typsca.eq.'C') then
                zc(iachlo-1+debugr-1+lggrel+1) = dcmplx(rnnem,rnnem)
            else if (typsca.eq.'I') then
                zi(iachlo-1+debugr-1+lggrel+1) = innem
            else if (typsca.eq.'K8') then
                zk8(iachlo-1+debugr-1+lggrel+1) = knnem
            else if (typsca.eq.'K16') then
                zk16(iachlo-1+debugr-1+lggrel+1) = knnem
            else if (typsca.eq.'K24') then
                zk24(iachlo-1+debugr-1+lggrel+1) = knnem
            else
                call assert(.false.)
            endif
10      continue
!
!
!
    else if (code.eq.'VERIF') then
!     ------------------------------------------------
!
!        -- CHAMPS "OUT" :
        arret = .false.
        np = nbpara(opt,te,'OUT')
        do 30 ipar = 1, np
            ecras=.false.
            nompar = nopara(opt,te,'OUT',ipar)
            iparg = indik8(zk8(iaoppa),nompar,1,npario)
            lgcata=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)
            if (lgcata .le. 0) goto 30
            ich=zi(iawloc-1+3*(iparg-1)+3)
            if (ich .eq. 0) goto 30
            iachlo=zi(iawloc-1+3*(iparg-1)+1)
            if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) goto 30
!
            typsca = zk8(iawtyp-1+iparg)
            lggrel=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+4)
            debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
!
!
            if (typsca .eq. 'R') then
                if (iisnan(zr(iachlo-1+debugr-1+lggrel+1)) .eq. 0) ecras=.true.
            else if (typsca.eq.'C') then
                if (iisnan(dble(zc(iachlo-1+debugr-1+lggrel+1))) .eq. 0) ecras=.true.
                if (iisnan(dimag(zc(iachlo-1+debugr-1+lggrel+1))) .eq. 0) ecras=.true.
            else if (typsca.eq.'I') then
                if (zi(iachlo-1+debugr-1+lggrel+1) .ne. innem) ecras= .true.
            else
                call assert(.false.)
            endif
!
            if (ecras) then
                arret = .true.
                call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
                call jenuno(jexnum('&CATA.OP.NOMOPT', opt), nomopt)
                valk(1) = nomte
                valk(2) = nomopt
                valk(3) = nompar
                call u2mesk('E', 'CALCULEL_42', 3, valk)
            endif
!
30      continue
!
        call assert(.not.arret)
!
    endif
!
!
end subroutine
