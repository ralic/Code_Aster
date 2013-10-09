subroutine ircmva(numcmp, ncmpve, ncmprf, nvalec, nbpg,&
                  nbsp, adsv, adsd, adsl, adsk,&
                  partie, tymast, modnum, nuanom, typech,&
                  val, profas, ideb, ifin, codret)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!_______________________________________________________________________
!     ECRITURE D'UN CHAMP -  FORMAT MED - CREATION DES VALEURS
!        -  -       -               -                  --
!_______________________________________________________________________
!     ENTREES :
!       NUMCMP : NUMEROS DES COMPOSANTES
!       NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
!       NVALEC : NOMBRE DE VALEURS A ECRIRE
!       NBPG   : NOMBRE DE POINTS DE GAUSS (1 POUR DES CHAMNO)
!       NBSP   : NOMBRE DE SOUS-POINTS (1 POUR DES CHAMNO)
!       TYPECH : TYPE DE CHAMP (ELEM,ELNO,ELGA,NOEU)
!       ADSV,D,L,K : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
!       PARTIE: IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!               UN CHAMP COMPLEXE
!       TYMAST : TYPE ASTER DE MAILLE QUE L'ON VEUT (0 POUR LES NOEUDS)
!       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
!                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
!                     MODNUM = 0 : NUMEROTATION IDENTIQUE
!                     MODNUM = 1 : NUMEROTATION DIFFERENTE
!       NUANOM : TABLEAU DE CORRESPONDANCE DES NOEUDS MED/ASTER.
!                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD DE LA
!                MAILLE DE TYPE ITYP DANS MED.
!       PROFAS : PROFIL ASTER. C'EST LA LISTE DES NUMEROS ASTER
!                DES NOEUDS OU DES ELEMENTS POUR LESQUELS LE CHAMP
!                EST DEFINI
!       IDEB   : INDICE DE DEBUT DANS PROFAS
!       IFIN   : INDICE DE FIN DANS PROFAS
!     SORTIES :
!       VAL    : VALEURS EN MODE ENTRELACE
!       CODRET : CODE RETOUR, S'IL VAUT 100, IL Y A DES COMPOSANTES
!                 MISES A ZERO
!_______________________________________________________________________
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: ncmpve, ncmprf, nvalec, nbpg, nbsp
    integer :: numcmp(ncmprf)
    integer :: adsv, adsd, adsl, adsk
    integer :: tymast, codret
    integer :: modnum(ntymax), nuanom(ntymax, *)
    integer :: profas(*)
    integer :: ideb, ifin
!
    real(kind=8) :: val(ncmpve, nbsp, nbpg, nvalec)
!
    character(len=8) :: typech
    character(len=*) :: partie
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    character(len=8) :: part, gd, valk(2), typcha
    integer :: iaux, jaux, kaux, itype
    integer :: adsvxx, adslxx
    integer :: ino, ima, nrcmp, nrcmpr, nrpg, nrsp
    integer :: ifm, nivinf
!
    logical :: logaux, lprolz
!
!====
! 1. PREALABLES
!====
    lprolz=.false.
    part=partie
    gd=zk8(adsk-1+2)
    codret=0
!
    call dismoi('TYPE_SCA', gd, 'GRANDEUR', repk=typcha)
!
    if (typcha .eq. 'R') then
        itype=1
    else if (typcha.eq.'C') then
        if (part(1:4) .eq. 'REEL') then
            itype=2
        else if (part(1:4).eq.'IMAG') then
            itype=3
        else
            ASSERT(.false.)
        endif
    else
        valk(1) = gd
        valk(2) = 'IRCMVA'
        call utmess('F', 'DVP_3', nk=2, valk=valk)
    endif
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
!
! 1.2. ==> INFORMATION
!
    if (nivinf .gt. 1) then
        call utmess('I', 'MED_47')
        write (ifm,13001) nvalec, ncmpve, nbpg, nbsp, typech
    endif
    13001 format('  NVALEC =',i8,', NCMPVE =',i8,&
     &       ', NBPG   =',i8,', NBSP   =',i8,/,&
     &       '  TYPECH =',a8)
!
!====
! 2. CREATION DU CHAMP DE VALEURS AD-HOC
!    LE TABLEAU DE VALEURS EST UTILISE AINSI :
!        TV(NCMPVE,NBSP,NBPG,NVALEC)
!    EN FORTRAN, CELA CORRESPOND AU STOCKAGE MEMOIRE SUIVANT :
!    TV(1,1,1,1), TV(2,1,1,1), ..., TV(NCMPVE,1,1,1),
!    TV(1,2,1,1), TV(2,2,1,1), ..., TV(NCMPVE,2,1,1),
!            ...     ...     ...
!    TV(1,NBSP,NBPG,NVALEC), TV(2,NBSP,NBPG,NVALEC), ... ,
!                                      TV(NCMPVE,NBSP,NBPG,NVALEC)
!    C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!    ATTENTION : LE CHAMP SIMPLIFIE EST DEJA PARTIELLEMENT FILTRE ...
!    ATTENTION ENCORE : LE CHAMP SIMPLIFIE N'A PAS LA MEME STRUCTURE
!    POUR LES NOEUDS ET LES ELEMENTS. IL FAUT RESPECTER CE TRAITEMENT
!    REMARQUE : SI UNE COMPOSANTE EST ABSENTE, ON AURA UNE VALEUR NULLE
!    REMARQUE : ATTENTION A BIEN REDIRIGER SUR LE NUMERO DE
!    COMPOSANTE DE REFERENCE
!====
!
! 2.1. ==> POUR LES NOEUDS : ON PREND TOUT CE QUI FRANCHIT LE FILTRE
!
    if (tymast .eq. 0) then
!GN        PRINT *,'PREMIER NOEUD : ',PROFAS(IDEB)
!GN        PRINT *,'DERNIER NOEUD : ',PROFAS(IFIN)
!
        do 21 , nrcmp = 1 , ncmpve
!
        adsvxx = adsv-1+numcmp(nrcmp)-ncmprf
        adslxx = adsl-1+numcmp(nrcmp)-ncmprf
        jaux = 0
        do 211 , iaux = ideb, ifin
        ino = profas(iaux)
        jaux = jaux + 1
        kaux = ino*ncmprf
        if (zl(adslxx+kaux)) then
            if (itype .eq. 1) then
                val(nrcmp,1,1,jaux) = zr(adsvxx+kaux)
            else if (itype.eq.2) then
                val(nrcmp,1,1,jaux) = dble(zc(adsvxx+kaux))
            else if (itype.eq.3) then
                val(nrcmp,1,1,jaux) = dimag(zc(adsvxx+kaux))
            endif
        else
            lprolz=.true.
            val(nrcmp,1,1,jaux) = 0.d0
        endif
211     continue
!
 21     continue
!
        if (lprolz) codret = 100
!
    else
!
! 2.2. ==> POUR LES MAILLES : ON PREND TOUT CE QUI FRANCHIT LE FILTRE
!          ET QUI EST DU TYPE EN COURS
!          REMARQUE : ON NE REDECODE PAS LES NOMBRES DE POINTS DE GAUSS
!          NI DE SOUS-POINT CAR ILS SONT INVARIANTS POUR UNE IMPRESSION
!          DONNE
!          REMARQUE : DANS LE CAS DE CHAMPS AUX NOEUDS PAR ELEMENTS,
!          L'ORDRE DE STOCKAGE DES VALEURS DANS UNE MAILLE DONNEE EST
!          CELUI DE LA CONNECTIVITE LOCALE DE LA MAILLE. OR POUR
!          CERTAINES MAILLES, CET ORDRE CHANGE ENTRE ASTER ET MED. IL
!          FAUT DONC RENUMEROTER.
!
!GN        PRINT *,'PREMIERE MAILLE : ',PROFAS(IDEB)
!GN        PRINT *,'DERNIERE MAILLE : ',PROFAS(IFIN)
!
! 2.2.1. ==> A-T-ON BESOIN DE RENUMEROTER ?
!            REMARQUE : LE MODE DE RANGEMENT FAIT QUE CELA NE FONCTIONNE
!            QUE POUR LES CHAMPS AVEC 1 SEUL SOUS-POINT.
!
        logaux = .false.
        if (typech(1:4) .eq. 'ELNO') then
            if (modnum(tymast) .eq. 1) then
                logaux = .true.
            endif
        endif
!
        if (logaux) then
            if (nbsp .gt. 1) then
                write (ifm,13001) nvalec, ncmpve, nbpg, nbsp
                call utmess('F', 'MED_48')
            endif
        endif
!
! 2.2.2. ==> TRANSFERT
!            ON FAIT LE TEST AVANT LA BOUCLE 211. IL EST DONC FAIT
!            AUTANT DE FOIS QUE DE COMPOSANTES A TRANSFERER. AU-DELA, CE
!            SERAIT AUTANT DE FOIS QUE DE MAILLES, DONC COUTEUX
!
        do 22 , nrcmp = 1 , ncmpve
!
        nrcmpr = numcmp(nrcmp)
        jaux = 0
        if (logaux) then
!
            nrsp = 1
            do 221 , iaux = ideb, ifin
            ima = profas(iaux)
            jaux = jaux + 1
            do 2211 , nrpg = 1 , nbpg
            call cesexi('C', adsd, adsl, ima, nrpg,&
                        nrsp, nrcmpr, kaux)
            if ((kaux.gt.0)) then
                if (itype .eq. 1) then
                    val(nrcmp,nrsp,nuanom(tymast,nrpg),&
                                jaux)= zr(adsv-1+kaux)
                else if (itype.eq.2) then
                    val(nrcmp,nrsp,nuanom(tymast,nrpg),&
                                jaux)= dble(zc(adsv-1+kaux))
                else if (itype.eq.3) then
                    val(nrcmp,nrsp,nuanom(tymast,nrpg),&
                                jaux)= dimag(zc(adsv-1+kaux))
                endif
            endif
2211         continue
!
221         continue
!
        else
!
            do 222 , iaux = ideb, ifin
            ima = profas(iaux)
            jaux = jaux + 1
            do 2221 , nrpg = 1 , nbpg
            do 2222 , nrsp = 1 , nbsp
            call cesexi('C', adsd, adsl, ima, nrpg,&
                        nrsp, nrcmpr, kaux)
            if ((kaux.gt.0)) then
                if (itype .eq. 1) then
                    val(nrcmp,nrsp,nrpg,jaux)=zr(adsv-&
                                    1+kaux)
                else if (itype.eq.2) then
                    val(nrcmp,nrsp,nrpg,jaux)=dble(zc(&
                                    adsv-1+kaux))
                else if (itype.eq.3) then
                    val(nrcmp,nrsp,nrpg,jaux)=dimag(&
                                    zc(adsv-1+kaux))
                endif
            endif
2222         continue
2221         continue
!
222         continue
!
        endif
!
 22     continue
!
    endif
!
end subroutine
