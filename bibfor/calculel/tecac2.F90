subroutine tecac2(stopz, numa, nmparz, louez, nval,&
                  itab, iret)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
!
!----------------------------------------------------------------------
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/chloet.h'
    include 'asterfort/contex.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: stopz, nmparz, louez
    character(len=8) :: nompar, stop8
    character(len=1) :: loue
    integer :: nval, itab(nval), iret, numa
!----------------------------------------------------------------------
!
!     BUT:
!     ---
!     OBTENIR DES INFORMATIONS SUR LE CHAMP LOCAL ASSOCIE A UN
!     PARAMETRE DANS UNE ROUTINE TE00IJ (OU EN DESSOUS)
!
!
!     ENTREES:
!     --------
! NUMA    : / 0  : ELEMENT COURANT
!           / >0 : NUMERO D'1 MAILLE DU MAILLAGE
! NMPARZ  : NOM DU PARAMETRE DE L'OPTION
! LOUEZ   : 'L' OU 'E'  ( LECTURE/ECRITURE )
! NVAL    : NOMBRE DE VALEURS DESIREES DANS ITAB(*)
! STOPZ   : PERMET DE DIRE A TECAC2 DE S'ARRETER SI ...
!
! 1) STOPZ(1:1) : SI LE PARAMETRE N'APPARAIT PAS DANS LE CATALOGUE
!                 DU TYPE_ELEMENT (OU CELUI DE L'OPTION)
!            'O'  -> ON S'ARRETE EN ERREUR <F>
!            'N'  -> ON NE S'ARRETE PAS
!                    => IRET=1 , ITAB(1)=0
!
! 2) STOPZ(2:2) : SI  LE CHAMP_LOCAL ASSOCIE AU PARAMETRE N'EXISTE PAS
!        CE QUI PEUT ARRIVER POUR 2 RAISONS :
!         2-1) LE PARAMETRE N'APPARTIENT PAS A LPAIN (OU LPAOUT)
!         2-2) LE CHAMP GLOBAL (CHIN) ASSOCIE AU PARAMETRE N'EXISTE PAS
!            'O'  -> ON S'ARRETE EN ERREUR <F>
!            'N'  -> ON NE S'ARRETE PAS
!                    => IRET=2 , ITAB(1)=0
!
! 3) STOPZ(3:3) : SI  LE CHAMP_LOCAL ASSOCIE AU PARAMETRE EST INCOMPLET
!        (I.E. ON N'A PAS PU EXTRAIRE TOUTES LES CMPS VOULUES)
!            'O'  -> ON S'ARRETE EN ERREUR <F>
!            'N'  -> ON NE S'ARRETE PAS
!                    => IRET=3,ITAB(1)=ADRESSE DU CHAMP LOCAL INCOMPLET
!                       POUR S'EN SORTIR IL FAUT UTILISER ITAB(8)
!                       REMARQUE : SI NVAL < 8, ON REND ITAB(1)=0 POUR
!                         EVITER D'UTILISER UNE ADRESSE INUTILISABLE
!
!
!
!
!
! SORTIES:
! --------
! IRET : CODE RETOUR :
!       0 -> TOUT OK
!       1 -> LE PARAMETRE N'EXISTE PAS DANS LE CATALOGUE DE L'ELEMENT
!       2 -> LE CHAMP N'EST PAS FOURNI PAR L'APPELANT DE CALCUL
!       3 -> LE CHAMP EST INCOMPLET : IL MANQUE DES CMPS
!
! ITAB(1)   : ADRESSE DU CHAMP_LOCAL (DANS ZR, ZC, ....)
!             = 0  SI IL N'EXISTE PAS DE CHAMP LOCAL (IRET=1,2)
!
!
! ITAB(2)   : LONGUEUR DU CHAMP_LOCAL DANS LE CATALOGUE
!             (NE TIENT PAS COMPTE DE NCDYN ET NBSPT
!              VOIR CI-DESSOUS ITAB(6) ET ITAB(7) )
! ITAB(3)   : NOMBRE DE POINTS DE LOCALISATION DU CHAMP
! ITAB(4)   : 9999 (INUTILISE)
! ITAB(5)   : TYPE_SCALAIRE DU CHAMP :
!             1 --> REEL
!             2 --> COMPLEXE
!             3 --> ENTIER
!             4 --> K8
!             5 --> K16
!             6 --> K24
! ITAB(6)   : NCDYN : NOMBRE DE CMP POUR LA GRANDEUR VARI_R
! ITAB(7)   : NBSPT : NOMBRE DE SOUS-POINTS
! ITAB(8)   : ADRESSE (DANS ZL) D'UN VECTEUR DE BOOLEENS
!             PARALLELE AU CHAMP LOCAL PERMETTANT DE SAVOIR QUELLES
!             SONT LES CMPS PRESENTES ET ABSENTES
!
!     -----------------------------------------------------------------
    logical :: exichl, etendu
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds, iaoppa
    integer :: iamloc, ilmloc, iadsgd
    integer :: npario, iel, iparg
    integer :: nparin, jceld, iachii, iachoi, adiel, debugr, nbspt, ncdyn
    integer :: lgcata, iachik, iachix, iachok, decael, lonchl, iachlo, ilchlo
    integer :: k, jrsvi, jcrsvi, i1, ich, iel2, igr2, debgr2
!
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    common /caii04/iachii,iachik,iachix
    common /caii07/iachoi,iachok
    common /caii08/iel
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
    character(len=24) :: valk(3)
    logical :: stpcat, stpexi, stpinc
!
!     DEB--------------------------------------------------------------
    call assert(numa.ge.0)
    if (numa .eq. 0) then
        igr2=igr
        iel2=iel
    else
        igr2=zi(jrepe-1+2*(numa-1)+1)
        iel2=zi(jrepe-1+2*(numa-1)+2)
    endif
!
    nompar = nmparz
    stop8 = stopz
    loue = louez
!
    stpcat = (stop8(1:1).eq.'O')
    stpexi = (stop8(2:2).eq.'O')
    stpinc = (stop8(3:3).eq.'O')
!
    call assert(loue.eq.'L' .or. loue.eq.'E')
    call assert(1.le.nval .and. nval.le.8)
    iret = 0
    itab(1) = 0
!
!
!     1- SI LE PARAMETRE N'APPARTIENT PAS A L'OPTION :
!     -------------------------------------------------
    exichl = .false.
!
    iparg = indik8(zk8(iaoppa),nompar,1,npario)
    if (iparg .eq. 0) then
        if (stpcat) then
            valk(1) = nompar
            valk(2) = option
            call u2mesk('E', 'CALCULEL2_69', 2, valk)
            call contex(option, ' ')
        endif
        iret = 1
        goto 20
    endif
!
!
!     2- SI LE PARAMETRE APPARTIENT A L'OPTION :
!     -------------------------------------------------
!
! --- ON VERIFIE QUE LES PARAMETRE IN SONT EN LECTURE
!     ET QUE LES PARAMETRES OUT SONT EN ECRITURE
    if (iparg .gt. nparin .and. loue .eq. 'L') then
        write(6,*)'PARAMETRE OUT EN LECTURE : ',nompar
        call assert(.false.)
    else if (iparg.le.nparin .and. loue.eq.'E') then
        write(6,*)'PARAMETRE IN EN ECRITURE : ',nompar
        call assert(.false.)
    endif
!
    iachlo=zi(iawloc-1+3*(iparg-1)+1)
    ilchlo=zi(iawloc-1+3*(iparg-1)+2)
    lgcata=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr2-1)+2)
    debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr2-1)+5)
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
    if (iachlo .eq. -1) iret = 2
    if (lgcata .eq. -1) iret = 1
!
!
!     -- SI IACHLO=-1    : LE CHAMP N'EXISTE PAS (GLOBALEMENT)
!     -- SI LGCATA=-1 : LE PARAMETRE N'EXISTE PAS POUR LE TYPE_ELEMENT
!     -------------------------------------------------
    if (iachlo .eq. -1) then
        if (stpexi) then
            valk(1) = nompar
            valk(2) = option
            valk(3) = nomte
            if (nompar(1:5) .eq. 'PVARC') then
                call u2mesk('F', 'CALCULEL4_10', 3, valk)
            else
                call u2mesk('E', 'CALCULEL4_95', 3, valk)
                call contex(option, nompar)
            endif
!
        endif
!
        if (lgcata .eq. -1) then
            if (stpcat) then
                valk(1) = nompar
                valk(2) = option
                valk(3) = nomte
                call u2mesk('E', 'CALCULEL2_70', 3, valk)
                call contex(option, nompar)
            endif
        endif
    else
        if (lgcata .eq. -1) then
            if (stpcat) then
                valk(1) = nompar
                valk(2) = option
                valk(3) = nomte
                call u2mesk('E', 'CALCULEL2_70', 3, valk)
                call contex(option, nompar)
            endif
        else
            exichl = .true.
        endif
    endif
!
!
!     -------------------------------------------------
!
    if (.not.exichl) then
        goto 20
    endif
!
!
!     ITAB(1) : ADRESSE DU CHAMP LOCAL POUR L'ELEMENT IEL2 :
!     -----------------------------------------------------
!
!     -- CALCUL DE ITAB(1),LONCHL,DECAEL,NBSPT,NCDYN :
!     -------------------------------------------------
    call chloet(iparg, etendu, jceld)
    if (etendu) then
        adiel = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+4)
        debgr2 = zi(jceld-1+zi(jceld-1+4+igr2)+8)
        nbspt = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+1)
        ncdyn = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+2)
        call assert(lgcata.eq.zi(jceld-1+zi(jceld-1+4+igr2)+3))
        decael = (adiel-debgr2)
        lonchl = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+3)
    else
        ncdyn = 0
        nbspt = 1
        if (jrsvi .eq. 0) then
            decael = (iel2-1)*lgcata
            lonchl = lgcata
        else
            i1 = zi(jcrsvi-1+igr2)
            decael = zi(jrsvi-1+i1-1+iel2)
            lonchl = zi(jrsvi-1+i1-1+iel2+1) - decael
            decael = decael -1
        endif
    endif
    itab(1) = iachlo+debugr-1+decael
!
!
!     -- POUR LES CHAMPS "IN" ON VERIFIE QUE L'EXTRACTION EST
!        COMPLETE SUR L'ELEMENT:
!     ----------------------------------------------------------
    if (ilchlo .ne. -1) then
        do 10,k = 1,lonchl
        if (.not.zl(ilchlo+debugr-1+decael-1+k)) then
            if (stpinc) then
                valk(1) = nompar
                valk(2) = option
                valk(3) = nomte
                call u2mesk('E', 'CALCULEL4_96', 3, valk)
                call contex(option, nompar)
            else
                iret = 3
                if (nval .lt. 8) itab(1)=0
            endif
        endif
10      continue
    endif
!
    if (nval .lt. 2) goto 20
!
!
!     ITAB(2) : LONGUEUR DU CHAMP LOCAL (CATALOGUE) :
!     -----------------------------------------------------
    itab(2) = lgcata
    if (nval .lt. 3) goto 20
!
!
!     ITAB(3) : NOMBRE DE POINTS (CATALOGUE) :
!     -----------------------------------------------------
    itab(3)=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr2-1)+3)
    if (nval .lt. 4) goto 20
    itab(4) = 9999
    if (nval .lt. 5) goto 20
!
!
!     ITAB(5) : TYPE DU CHAMP LOCAL  :
!           R/C/I/K8/K16/K24
!           1/2/3/4 /5  /6
!     -----------------------------------------------------
    if (zk8(iawtyp-1+iparg) (1:1) .eq. 'R') then
        itab(5) = 1
    else if (zk8(iawtyp-1+iparg) (1:1).eq.'C') then
        itab(5) = 2
    else if (zk8(iawtyp-1+iparg) (1:1).eq.'I') then
        itab(5) = 3
    else if (zk8(iawtyp-1+iparg) (1:3).eq.'K8 ') then
        itab(5) = 4
    else if (zk8(iawtyp-1+iparg) (1:3).eq.'K16') then
        itab(5) = 5
    else if (zk8(iawtyp-1+iparg) (1:3).eq.'K24') then
        itab(5) = 6
    else
        call assert(.false.)
    endif
    if (nval .lt. 6) goto 20
!
!
!     ITAB(6) : NCDYN : NOMBRE DE CMP POUR LA GRANDEUR VARI_R (IEL2)
!     -------------------------------------------------------
    itab(6) = ncdyn
    if (nval .lt. 7) goto 20
!
!
!     ITAB(7) : NBSPT : NOMBRE DE SOUS-POINTS POUR IEL2
!     -----------------------------------------------------
    itab(7) = nbspt
    if (nval .lt. 8) goto 20
!
!
!     ITAB(8) : ADRESSE DU VECTEUR DE BOOLEENS :
!     -----------------------------------------------------
    itab(8) = ilchlo+debugr-1+decael
    if (nval .lt. 9) goto 20
!
!
!
20  continue
!
end subroutine
