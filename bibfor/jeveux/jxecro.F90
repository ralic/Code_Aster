subroutine jxecro(ic, iadmi, iaddi, lso, idco,&
                  idos)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
#include "asterf_types.h"
#include "jeveux_private.h"
#include "asterfort/jxdeps.h"
#include "asterfort/jxecrb.h"
#include "asterfort/jxlirb.h"
#include "asterfort/jxouvr.h"
#include "asterfort/utmess.h"
    integer :: ic, iadmi, iaddi(2), lso, idco, idos
! ----------------------------------------------------------------------
! ECRITURE D'UN SEGMENT DE VALEUR
!
! IN  IC    : NOM DE LA CLASSE
! IN  IADMI : ADRESSE MEMOIRE DU SEGMENT DE VALEURS
! VAR IADDI : ADRESSE DISQUE DU SEGMENT DE VALEURS
! IN  LSO   : LONGUEUR EN OCTET DU SEGMENT DE VALEURS
! IN  IDCO  : IDENTIFICATEUR DE COLLECTION
! IN  IDOS  : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jadm, iso, jiecr, jusadi, k, kd
    integer :: kl, ladm, lsa, lsadi, n, nbl, numdeb
    integer :: numext
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    aster_logical :: litlec
    common /lficje/  litlec(n)
    integer :: idn, iext, nbenrg
    common /iextje/  idn(n) , iext(n) , nbenrg(n)
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    common /jusadi/  jusadi(n)
!     ------------------------------------------------------------------
    integer :: iadmo, kadd, ladd, nde, lgbl, lso2
    aster_logical :: lpetit
    parameter      ( nde = 6)
! ----------------------------------------------------------------------
! REMARQUE : LE PARAMETER NDE EST AUSSI DEFINI DANS JXLIRO
! ----------------------------------------------------------------------
! DESCRIPTION D'UN ENREGISTREMENT:
! ______________________________________________________________________
! I      I      I      I      I     I                       I      I
! I IDCO I IDOS I IDC1 I IDS1 I LS1 I     ............      I IDC2 I ...
! I______I______I______I______I_____I_______________________I______I____
!
! IDCO   : IDENTIFICATEUR DE COLLECTION
! IDOS   : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
!        - SI L'UN DES DEUX EST NON NUL L'ENREGISTREMENT CORRESPOND A
!          UN "GROS" OBJET
!        - SI LES DEUX SONT NULS L'ENREGISTREMENT CONTIENT PLUSIEURS
!          SEGMENTS DE VALEURS ASSOCIES A DE "PETITS" OBJETS
!          DANS CE CAS :
! IDC1   : IDENTIFICATEUR DE COLLECTION DE L'OBJET 1
! IDS1   : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION DE
!          L'OBJET 1
! LS1    : LONGUEUR DU SEGMENT DE VALEURS ASSOCIE A L'OBJET 1
!          SUIVENT ENSUITE LES VALEURS
!          SI IDCI EST NUL, ON A ATTEINT LA FIN DE L'ENREGISTREMENT
!
! DEB ------------------------------------------------------------------
    jadm = iadmi
    ladm = iszon(jiszon + jadm - 3 )
    kadd = iaddi(1)
    ladd = iaddi(2)
    iadmo = ( jadm - 1 ) * lois + ladm + 1
    lgbl = 1024*longbl(ic)*lois
    lso2 = lso
    if (mod(lso,lois) .ne. 0) lso2 = (1 + lso/lois) * lois
    lpetit = ( lso2 .lt. lgbl-nde*lois )
    if (iaddi(1) .eq. 0) then
!
! ----- PREMIER DECHARGEMENT
!
        if (lpetit) then
!
! -------- PETIT OBJET
!
            if (nitecr(ic) + lso2 +nde*lois .gt. lgbl) then
!
! --------- LE PETIT OBJET NE TIENT PAS DANS LE TAMPON D'ECRITURE
                if (iitecr(ic) .gt. 0) then
                    jiecr = (jk1zon+kitecr(ic)+nitecr(ic))/lois+1
!
! ----------- ON STOCKE LA LONGUEUR RESTANTE DE L'ENREGISTREMENT AU BOUT
                    iszon(jiecr ) = 0
                    iszon(jiecr+1) = 0
                    iszon(jiecr+2) = (lgbl-nitecr(ic))/lois-3
                    call jxecrb(ic, iitecr(ic), kitecr(ic)+1, lgbl, 0,&
                                0)
                endif
                do 101 kd = 1, nblmax(ic)
                    lsadi = jusadi(ic)+3*kd-2
                    iso = iusadi(lsadi)+iusadi(lsadi+1)
                    if (iso .ge. 0) goto 101
                    nbluti(ic) = max (kd,nbluti(ic))
                    numext = (nbluti(ic)-1)/nbenrg(ic)
                    if (numext .gt. iext(ic)-1) then
                        numdeb = iext(ic)
                        do 103 k = numdeb, numext
                            call jxouvr(ic, k+1)
                            iext(ic) = iext(ic)+1
103                     continue
                    endif
                    goto 104
101             continue
                call utmess('F', 'JEVEUX_42', sk=nombas(ic), si=nblmax(ic))
104             continue
                iitecr(ic) = kd
                iusadi(jusadi(ic)+ 3*iitecr(ic)-2) = 0
                iusadi(jusadi(ic)+ 3*iitecr(ic)-1) = 0
                iusadi(jusadi(ic)+ 3*iitecr(ic) ) = 0
                nitecr(ic) = 0
            else
                if (iitecr(ic) .eq. 0) then
                    do 201 kd = 1, nblmax(ic)
                        lsadi = jusadi(ic)+3*kd-2
                        iso = iusadi(lsadi)+iusadi(lsadi+1)
                        if (iso .ge. 0) goto 201
                        nbluti(ic) = max (kd,nbluti(ic))
                        numext = (nbluti(ic)-1)/nbenrg(ic)
                        if (numext .gt. iext(ic)-1) then
                            numdeb = iext(ic)
                            do 203 k = numdeb, numext
                                call jxouvr(ic, k+1)
                                iext(ic) = iext(ic)+1
203                         continue
                        endif
                        goto 204
201                 continue
                    call utmess('F', 'JEVEUX_42', sk=nombas(ic), si=nblmax( ic))
204                 continue
                    iitecr(ic) = kd
                    iusadi(jusadi(ic)+ 3*iitecr(ic)-2) = 0
                    iusadi(jusadi(ic)+ 3*iitecr(ic)-1) = 0
                    iusadi(jusadi(ic)+ 3*iitecr(ic) ) = 0
                    nitecr(ic) = 0
                endif
            endif
            jiecr = (jk1zon+kitecr(ic)+nitecr(ic))/lois+1
            iszon(jiecr ) = idco
            iszon(jiecr+1) = idos
            iszon(jiecr+2) = lso2/lois
            iszon(jiecr+2+(lso2/lois)+1) = 0
            iszon(jiecr+2+(lso2/lois)+2) = 0
            iszon(jiecr+2+(lso2/lois)+3) = 0
            call jxdeps(iadmo, kitecr(ic)+nitecr(ic)+3*lois+1, lso)
            iaddi(1) = iitecr(ic)
            iaddi(2) = nitecr(ic)+3*lois
            nitecr(ic) = nitecr(ic) + lso2 + 3*lois
        else
!
! ------- GROS OBJET
!
            nbl = lso2 / lgbl
            if (mod ( lso2 , lgbl ) .ne. 0) nbl = nbl + 1
            kd = 1
301         continue
            if (kd .le. nblmax(ic)-nbl) then
                lsadi = jusadi(ic)+3*kd-2
                do 302 kl = 1, nbl
                    lsa = lsadi+3*(kl-1)
                    iso = iusadi(lsa)+iusadi(lsa+1)
                    if (iso .ge. 0) then
                        kd = kd + kl
                        goto 301
                    endif
302             continue
                iaddi(1) = kd
                nbluti(ic) = max (kd+nbl-1,nbluti(ic))
                numext = (nbluti(ic)-1)/nbenrg(ic)
                if (numext .gt. iext(ic)-1) then
                    numdeb = iext(ic)
                    do 303 k = numdeb, numext
                        call jxouvr(ic, k+1)
                        iext(ic) = iext(ic)+1
303                 continue
                endif
                goto 304
            endif
            call utmess('F', 'JEVEUX_42', sk=nombas(ic), si=nblmax(ic))
304         continue
            call jxecrb(ic, kd, iadmo, lso2, idco,&
                        idos)
        endif
    else
!
! ----- DECHARGEMENTS ULTERIEURS
!
        if (lpetit) then
!
! -------- PETIT OBJET
!
            if (kadd .eq. iitlec(ic)) then
                call jxdeps(iadmo, kitlec(ic)+ladd+1, lso)
                litlec(ic) = .true.
            else if (kadd .eq. iitecr(ic)) then
                call jxdeps(iadmo, kitecr(ic)+ladd+1, lso)
            else
                if (litlec(ic)) then
                    call jxecrb(ic, iitlec(ic), kitlec(ic)+1, lgbl, 0,&
                                0)
                endif
                call jxlirb(ic, kadd, kitlec(ic)+1, lgbl)
                call jxdeps(iadmo, kitlec(ic)+ladd+1, lso)
                iitlec(ic) = kadd
                litlec(ic) = .true.
            endif
        else
!
! ------- GROS  OBJET
!
            call jxecrb(ic, kadd, iadmo, lso2, idco,&
                        idos)
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
