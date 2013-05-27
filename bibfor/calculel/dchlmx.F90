subroutine dchlmx(opt, ligrel, iparg, nin, lpain,&
                  nout, lpaout, taille)
    implicit none
!
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
    include 'asterc/indik8.h'
    include 'asterfort/digde2.h'
    include 'asterfort/modatt.h'
    include 'asterfort/nbelem.h'
    include 'asterfort/nbpara.h'
    include 'asterfort/nopara.h'
    include 'asterfort/typele.h'
    integer :: opt, nin, nout, taille, iparg
    character(len=19) :: ligrel
    character(len=8) :: lpain(*), lpaout(*)
! ----------------------------------------------------------------------
!
!     SORTIES:
!     TAILLE: DIMENSION DU CHAMP_LOC (NOMPAR)
!             TIENT COMPTE DES CASES "UNDEF"
!             =0 => AUCUN TYPE_ELEM NE CONNAIT LE PARAMETRE NOMPAR
! ----------------------------------------------------------------------
!
    common /caii04/iachii,iachik,iachix
    common /caii07/iachoi,iachok
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
    integer :: max
    integer :: iachii, iachik, iachix, iachoi, iachok
    integer :: iparin, iparou, jceld, debugr
    integer :: npin, npou, te, ipar, nval, mode
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds, iaoppa
    integer :: npario, nparin, iamloc, ilmloc, iadsgd
    integer :: nbsp, ncdyn, jel, taill1, jrsvi, jcrsvi
    character(len=8) :: tych, nopare, nompar
!
! DEB-------------------------------------------------------------------
!
    nompar = zk8(iaoppa-1+iparg)
    taill1 = 0
    taille = 0
    debugr=1
    do 30 igr = 1, nbgr
        te = typele(ligrel,igr)
        nbelgr = nbelem(ligrel,igr)
        npin = nbpara(opt,te,'IN ')
        npou = nbpara(opt,te,'OUT')
!
!           ---IN:
!           ------
        do 10 ipar = 1, npin
            nopare = nopara(opt,te,'IN ',ipar)
            if (nopare .eq. nompar) then
                iparin = indik8(lpain,nompar,1,nin)
                mode = modatt(opt,te,'IN ',ipar)
                nval = digde2(mode)
                tych = zk8(iachik-1+2* (iparin-1)+1)
!
!           CAS DES CHAM_ELEM POTENTIELLEMENT ETENDUS :
                if (tych(1:4) .eq. 'CHML') then
                    jceld = zi(iachii-1+11* (iparin-1)+4)
!
!             CAS DES CHAM_ELEM ETENDUS :
                    if ((zi(jceld-1+3).gt.1) .or. (zi(jceld-1+4).gt.1)) then
                        taill1=0
                        do 11, jel=1,nbelgr
                        nbsp = zi(jceld-1+zi(jceld-1+4+igr)+4+4*( jel-1)+1)
                        ncdyn = zi(jceld-1+zi(jceld-1+4+igr)+4+4*( jel-1)+2)
                        nbsp =max(nbsp,1)
                        ncdyn=max(ncdyn,1)
                        taill1=taill1+nval*ncdyn*nbsp
11                      continue
!
!             CAS DES CHAM_ELEM NON ETENDUS :
                    else
                        taill1=nval*nbelgr
                    endif
                else
                    taill1=nval*nbelgr
                endif
                goto 29
            endif
10      continue
!
!           ---OUT:
!           ------
        do 20 ipar = 1, npou
            nopare = nopara(opt,te,'OUT',ipar)
            if (nopare .eq. nompar) then
                iparou = indik8(lpaout,nompar,1,nout)
                mode = modatt(opt,te,'OUT',ipar)
                nval = digde2(mode)
                tych = zk8(iachok-1+2* (iparou-1)+1)
!
                if (tych(1:4) .eq. 'CHML') then
!             -- CAS DES CHAM_ELEM :
                    jceld = zi(iachoi-1+3*(iparou-1)+1)
                    taill1 = zi(jceld-1+zi(jceld-1+4+igr)+4)
                else
!             -- CAS DES RESUELEM :
                    taill1 = nval*nbelgr
                    if (evfini .eq. 1) then
                        jrsvi = zi(iachoi-1+3*(iparou-1)+2)
                        if (jrsvi .ne. 0) then
                            jcrsvi = zi(iachoi-1+3*(iparou-1)+3)
                            taill1 = zi(jrsvi-1+zi(jcrsvi-1+igr)+ nbelgr)-1
                        endif
                    endif
                endif
                goto 29
            endif
20      continue
29      continue
!
!
!
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+4)=taill1
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)=debugr
        if (calvoi .eq. 0) then
            taille = max(taille,taill1)
        else
            taille = taille+taill1
            debugr=debugr+taill1+1
        endif
!
30  end do
!
!     -- ON AJOUTE QUELQUES CASES POUR "UNDEF" :
    if (taille .gt. 0) then
        if (calvoi .eq. 0) then
            taille = taille+1
        else
            taille = taille+nbgr
        endif
    endif
!
!
end subroutine
