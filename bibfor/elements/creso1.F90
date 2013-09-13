subroutine creso1(solveu, method, preco, renum, syme,&
                  sdfeti, eps, resire, tbloc, nprec,&
                  nmaxit, istop, niremp, ifm, numsd,&
                  nbma, verif, testco, nbreor, tyreor,&
                  scalin, inumsd, imail, infofe, stogi,&
                  testok, nbreoi, acma, acsm, reacre)
!-----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    BUT :  REMPLISSAGE DE LA SD SOLVEUR FETI (NIVEAU 2)
!-----------------------------------------------------------------------
! IN SOLVEU   : NOM DE LA SD SOLVEUR
! IN METHOD....NIREMP : SES PARAMETRES, POUR SIGNIFICATION VOIR CRESOL.
! IN IFM     : PARAMETRE POUR MONITORING.
! IN NUMSD   : NUMERO DE SOUS-DOMAINE
! IN NBMA    : NBRE DE MAILLES
! IN VERIF   : PARAMETRE POUR DEMANDER LA VERIFICATION DE SDFETI
! IN TESTCO  : PARAMETRE POUR TESTER LA CONTINUITE
! IN NBREOR/TYREOR : PARAMETRES POUR LA REOTHOGONALISATION DES
!              DIRECTIONS DE DESCENTE AVEC FETI
! IN SCALIN  : PARAMETRE DE SCALING POUR LE PRECONDITIONNEMENT FETI
! IN INUMSD : ADRESSE OBJET JEVEUX '&FETI.MAILLE.NUMSD' DESIGNANT
!   LE NUMERO DE SOUS-DOMAINE D'APPARTENANCE D'UNE MAILLE PHYSIQUE DU
!   MODELE DONNEE
! IN IMAIL   : ADRESSE JEVEUX DE L'OBJET MODELE//'.MAILLE'
! IN INFOFE  : NIVEAU DE MONITORING POUR FETI
! IN STOGI   : PARAMETRE DE STOCKAGE DE LA MATRICE GI
! IN TESTOK  : FLAG SUR LA MISE EN OEUVRE DE TESTS DE COHERENCE
! IN NBREOI  : NBRE DE PAS DE TEMPS UTLILISES POUR REORTHO SI LIST_INST
! IN ACMA/ACSM : ACCELERATION POUR PB A MULTIPLES MATRICES ET
!               SECONDS MEMBRES
! IN REACRE  : FREQUENCE DE REACTUALISATION DU RESIDU
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/sdsolv.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: zslvk, zslvr, zslvi
    integer :: nprec, nmaxit, istop, niremp, ifm, numsd, nbma, nbreor, inumsd
    integer :: imail, nbreoi, reacre
    real(kind=8) :: eps, resire, tbloc, testco
    character(len=3) :: syme
    character(len=8) :: preco, renum, verif, tyreor, scalin, stogi, acma, acsm
    character(len=16) :: method
    character(len=19) :: solveu
    character(len=24) :: sdfeti, infofe
    logical :: testok
!
    integer :: vali(2)
!
! DECLARATION VARIABLES LOCALES
    integer :: islvk, islvr, islvi, i, nbmasd, ifeta, ibuff
    character(len=24) :: nomlog, sdfeta
!
!------------------------------------------------------------------
    call jemarq()
!
! --------------------------------------------------------------
! CREATION DES DIFFERENTS ATTRIBUTS DE LA S.D. SOLVEUR
! --------------------------------------------------------------
    zslvk = sdsolv('ZSLVK')
    zslvr = sdsolv('ZSLVR')
    zslvi = sdsolv('ZSLVI')
    call wkvect(solveu(1:19)//'.SLVK', 'V V K24', zslvk, islvk)
    call wkvect(solveu(1:19)//'.SLVR', 'V V R', zslvr, islvr)
    call wkvect(solveu(1:19)//'.SLVI', 'V V I', zslvi, islvi)
!
! --------------------------------------------------------------
! REMPLISSAGE DE LA SD SOLVEUR
! --------------------------------------------------------------
    zk24(islvk-1+1) = method
    zk24(islvk-1+2) = preco
    zk24(islvk-1+3) = verif
    zk24(islvk-1+4) = renum
    zk24(islvk-1+5) = syme
    zk24(islvk-1+6) = sdfeti
    zk24(islvk-1+7) = tyreor
    zk24(islvk-1+8) = scalin
    zk24(islvk-1+9) = stogi
    zk24(islvk-1+10) = acma
    zk24(islvk-1+11) = acsm
    zk24(islvk-1+12) = 'XXXX'
!
    zr(islvr-1+1) = eps
    zr(islvr-1+2) = resire
    zr(islvr-1+3) = tbloc
    zr(islvr-1+4) = testco
!
    zi(islvi-1+1) = nprec
    zi(islvi-1+2) = nmaxit
    zi(islvi-1+3) = istop
    zi(islvi-1+4) = niremp
    zi(islvi-1+5) = nbreor
    zi(islvi-1+6) = nbreoi
    zi(islvi-1+7) = reacre
    zi(islvi-1+8) = 0
!
!
! --------------------------------------------------------------
! SI ON UTILISE FETI
! --------------------------------------------------------------
! MONITORING
    if (method(1:4) .eq. 'FETI') then
        if (infofe(1:1) .eq. 'T') then
            if (numsd .eq. 0) then
                write (ifm,*) '<FETI/CRESO1> DOMAINE GLOBAL'
!
            else
                write (ifm,*) '<FETI/CRESO1> SD: ',numsd
            endif
        endif
        if (infofe(2:2) .eq. 'T') call utimsd(ifm, 2, .false., .true., solveu(1:19),&
                                              1, 'V')
    endif
!
! REMPLISSAGE DU VECTEUR AUXILIAIRE '&FETI.MAILLE.NUMSD'
    if (numsd .ne. 0) then
        sdfeta = sdfeti(1:19)//'.FETA'
        call jelira(jexnum(sdfeta, numsd), 'LONMAX', nbmasd)
        call jeveuo(jexnum(sdfeta, numsd), 'L', ifeta)
        do 10 i = 1, nbmasd
            ibuff = zi(ifeta+i-1)
            if (zi(inumsd+ibuff-1) .gt. 0) then
! MAILLE COMMUNE A PLUSIEURS SOUS-DOMAINES
                vali (1) = ibuff
                call utmess('F', 'ALGORITH16_98', si=vali(1))
            else
                zi(inumsd+ibuff-1) = numsd
            endif
10      continue
!
    else if ((numsd.eq.0) .and. (method(1:4).eq.'FETI')) then
        if (infofe(2:2) .eq. 'T') then
            nomlog = '&FETI.MAILLE.NUMSD'
            call utimsd(ifm, 2, .false., .true., nomlog(1:19),&
                        1, 'V')
        endif
        if (testok) then
! VERIFICATION QUE TOUTES LES MAILLES PHYSIQUES DU MODELE SONT DANS UN
! SOUS-DOMAINE SI ON EST EN SEQUENTIEL
            do 20 i = 1, nbma
                ibuff = zi(inumsd+i-1)
                if (ibuff .lt. 0) then
! MAILLE PAS DANS LA PARTITION FETI
                    if (zi(imail-1+i) .ne. 0) then
! MAILLE POURTANT DANS LE MODELE
                        vali (1) = i
                        call utmess('F', 'ELEMENTS4_76', si=vali(1))
                    endif
                endif
20          continue
        endif
    endif
!
! FIN ------------------------------------------------------
    call jedema()
end subroutine
