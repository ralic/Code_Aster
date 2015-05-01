subroutine lrvcpg(idfimd, nbpgm, nbpga, nomtm, typgeo,&
                  elrefa, fapg, nloc, locnam, permu,&
                  nutyma, nbsp, codret)
!
! person_in_charge: nicolas.sellenet at edf.fr
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
!-----------------------------------------------------------------------
!     LECTURE FICHIER MED - VERIFICATION ET COMPARAISON DES PG ASTER/MED
!     -    -                -               -               --
!-----------------------------------------------------------------------
!
!     ROUTINE APPELEE PAR: LRMPGA
!
!     IN :
!       IDFIMD : IDENTIFIANT DU FICHIER MED
!       NBPGM  : NOMBRE DE PG MED
!       NBPGA  : NOMBRE DE PG ASTER
!       ELREFA : NOM DE L'ELEMENT DE REFERENCE ASTER
!       ELREFM : NOM DE L'ELEMENT DE REFERENCE MED
!       FAPG   : FAMILLE DE PG GLOBALE
!       NLOC   : NOMBRE DE LOCALISATIONS PRESENTES DANS LE FICHIER MED
!    IN/OUT:
!       PERMU  : TABLEAU (EVENTUEL) DES PERMUTATIONS DES PG
!                PERMU(I_PG_MED)=I_PG_ASTER
!    OUT :
!       NUTYMA : NUMERO DU TYPE DE MAILLE DE ELREFA
!       CODRET : CORRESPONDANCE DES PG ASTER/MED
!                 CODRET=0 -->  OK
!                 CODRET=1 -->  NECESSITE DES PERMUTATIONS
!                 CODRET=2 -->  NOOK (SOIT ABSENCE DE LOCALISATION, SOIT
!                                     AUCUNE CORRESPONDANCE POSSIBLE )
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/as_mlclci.h"
#include "asterfort/as_mlclor.h"
#include "asterfort/assert.h"
#include "asterfort/elraga.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: tygeos, nbpgm, nbpga, nloc, nutyma, idfimd
    integer :: permu(nbpgm), codret, nbsp
    character(len=8) :: elrefa, fapg, nomtm
    character(len=64) :: locnam
!
    integer :: vali(2), jcopga, jwpga, ndim, nbfpg, iloc, dime, nbpgm2
    integer :: typgeo, nbpg, iret, nnoref, npgref, jrefco, jgscoo, jwg, jcorre
    integer :: ncorre, igau, idim, ad, ipgm, ipga, ada, im, ifm, nivinf
    character(len=8) :: valk(3)
    character(len=64) :: locnam2, nomasu
    integer :: edfuin
    parameter (edfuin=0)
    real(kind=8) :: xpgm, ypgm, zpgm, xpga, ypga, zpga, valr(2)
!
    data valk / 'X','Y','Z'/
!
    call jemarq()
!
    call infniv(ifm, nivinf)
    if ( locnam(23:64).ne.' ' ) then
        read(locnam(23:28),'(I6)') nbsp
        nbpgm2 = nbpgm/nbsp
    else
        nbsp = 1
        nbpgm2 = nbpgm
    endif
!
!     DETERMINATION DES COORDONNES DES PG
!     DE L'ELEMENT DE REFERENCE ASTER
!     -------------------------------
    call wkvect('&&LRVCPG_COORD_PG_ASTER', 'V V R', 3*nbpga, jcopga)
    call wkvect('&&LRVCPG_POIDS_PG_ASTER', 'V V R', nbpga, jwpga)
    call elraga(elrefa, fapg, dime, nbfpg, zr(jcopga),&
                zr(jwpga))
!
!     NUMERO DU TYPE DE MAILLE DE L'ELEMENT DE REFERENCE : NUTYMA
    call jenonu(jexnom('&CATA.TM.NOMTM', nomtm), nutyma)
!
!     VERIFICATION SUR LE NOMBRE DE POINTS DE GAUSS
!     ---------------------------------------------
!     ATTENTION: POUR LES CHAMPS A SOUS-POINTS,
!     LE NBRE DE PG D'UN ELEMENT DE REF MED
!     PREND EN COMPTE LE NOMBRE DE SOUS-POINTS
!     CECI PEUT ETRE LA CAUSE DE L'EMISSION
!     DU MESSAGE CI-DESSOUS
    if (nbpgm2 .ne. nbpga) then
        vali(1)=nbpgm
        vali(2)=nbpga
        call utmess('A', 'MED_2', ni=2, vali=vali)
        codret = 4
        goto 9999
    endif
!
!     DETERMINATION DU NOM DE LA LOCALISATION DES PG PRESENTE
!     DANS LE FICHIER MED ET CORRESPONDANT A L'ELEM DE REF ASTER.
!     ----------------------------------------------------------
!     -SI LA LOCALISATION EST ABSENTE : ON PREND EN COMPTE LA
!      LOCALISATION ASTER --> RISQUE DE RESULTATS FAUX
!     -SI LA LOCALISATION EST PRESENTE, ON COMPARE LES
!      COORDONNES DES PG ASTER/MED
    if (nivinf .gt. 1) then
        write(ifm,1001) nloc
    endif
    do iloc = 1, nloc
        call as_mlclci(idfimd, iloc, locnam2, tygeos, nbpg,&
                       ndim, nomasu, iret)
        if ( tygeos.eq.typgeo ) then
            if ( locnam.eq.' ' .or. locnam.eq.locnam2 ) goto 140
        endif
    end do
!     SI ON EST ICI, CELA SIGNIFIE QU'AUCUNE LOCALISATION
!     N'A ETE IDENTIFIEE POUR L'ELEMENT DE REFERENCE EN COURS
    if (nbpga .ne. 1 .or. nbpgm .ne. 1) then
        call utmess('A', 'MED_1', sk=elrefa)
    endif
    codret=2
    goto 9999
140 continue
!
!
    if (nivinf .gt. 1) then
        write(ifm,1002) locnam
    endif
!
!     DETERMINATION DES COORDONNES DES PG
!     DE L'ELEMENT DE REFERENCE MED
!     -------------------------------
    nnoref=(typgeo/100)*mod(typgeo,100)
    npgref=(typgeo/100)*nbpgm
    call wkvect('&&LRVCPG_COORD_NO_MED', 'V V R', nnoref, jrefco)
    call wkvect('&&LRVCPG_COORD_PG_MED', 'V V R', npgref, jgscoo)
    call wkvect('&&LRVCPG_POIDS_PG_MED', 'V V R', nbpgm, jwg)
    call as_mlclor(idfimd, zr(jrefco), zr(jgscoo), zr(jwg), edfuin,&
                   locnam, iret)
    ASSERT(typgeo/100.eq.dime)
!
!     COMPARAISON DES COORD DES PG ENTRE ASTER ET MED
!     -----------------------------------------------
!     NOMBRE DE PG NON APPARENTES : NCORRE
!     TABLEAU DE TRAVAIL ZI(JCORRE) DIMENSIONNE AU NBRE DE PG QUI VAUT:
!        -LE NUMERO DU PG LOCAL SI LA CORRESPONDANCE N'A PAS EU LIEU
!        -0 SINON
!
    call wkvect('&&LRVCPG_CORRESP_PG', 'V V I', nbpgm2, jcorre)
    ncorre=0
    do 100 igau = 1, nbpgm2
        zi(jcorre+igau-1)=0
        do idim = 1, dime
            ad=dime*(igau-1)+idim
            if (nivinf .gt. 1) then
                write(ifm,1100) igau,idim,zr(jgscoo+ad-1),zr(jcopga+&
                ad-1)
            endif
            if (abs(zr(jgscoo+ad-1)-zr(jcopga+ad-1)) .gt. 1.d-3) then
                ncorre=ncorre+1
                zi(jcorre+igau-1)=igau
                goto 100
            endif
        end do
100  end do
!
!     SI LES PG ASTER/MED CORRESPONDENT : TOUT VA BIEN
    if (ncorre .eq. 0) then
        codret=0
        goto 9999
    else
!        .. SINON, ON RECHERCHE UNE EVENTUELLE PERMUTATION:
!        PERMU = LE TABLEAU DE PERMUTATIONS DIMENSIONNE
!        AU NBRE DE PG: PERMU(NUM_PG_MED)=NUM_PG_ASTER
        do ipgm = 1, nbpgm2
            permu(ipgm)=0
            if (zi(jcorre+ipgm-1) .eq. 0) then
                permu(ipgm)=ipgm
            else
                ad=dime*(ipgm-1)
                xpgm=zr(jgscoo+ad+1-1)
                ypgm=0.d0
                zpgm=0.d0
                if (dime .ge. 2) ypgm=zr(jgscoo+ad+2-1)
                if (dime .ge. 3) zpgm=zr(jgscoo+ad+3-1)
                do ipga = 1, nbpgm2
                    ada=dime*(ipga-1)
                    xpga=zr(jcopga+ada+1-1)
                    ypga=0.d0
                    zpga=0.d0
                    if (dime .ge. 2) ypga=zr(jcopga+ada+2-1)
                    if (dime .ge. 3) zpga=zr(jcopga+ada+3-1)
                    if (abs(xpgm-xpga) .lt. 1.d-3 .and. abs(ypgm-ypga) .lt. 1.d-3 .and.&
                        abs(zpgm-zpga) .lt. 1.d-3) then
                        permu(ipgm)=ipga
                        codret=1
                        goto 200
                    endif
!                 SI ON EST ICI, CELA SIGNIFIE QUE L'UN DES PG MED
!                 N'A PAS PU ETRE IDENTIFIE A L'UN DES PG ASTER
!                 --> INCOMPATIBILITE DES PG, RISQUE DE RESULTATS FAUX
                    if (ipga .eq. nbpgm2) then
                        do im = 1, nbpgm2
                            call utmess('A+', 'MED_4', si=im)
                            do idim = 1, dime
                                valr(1)=zr(jgscoo+dime*(im-1)+idim-1)
                                valr(2)=zr(jcopga+dime*(im-1)+idim-1)
                                call utmess('A+', 'MED_5', sk=valk(idim), nr=2, valr=valr)
                            enddo
                        enddo
                        call utmess('A', 'MED_3')
                        codret=2
                        goto 9999
                    endif
                enddo
            endif
200      enddo
    endif
!
    if (codret .eq. 1) then
!        AFFICHAGE DES COORD DES PG MED/ASTER POUR
!        METTRE EN EVIDENCE LES PERMUTATIONS
        do im = 1, nbpgm2
            call utmess('A+', 'MED_4', si=im)
            do idim = 1, dime
                valr(1)=zr(jgscoo+dime*(im-1)+idim-1)
                valr(2)=zr(jcopga+dime*(im-1)+idim-1)
                call utmess('A+', 'MED_5', sk=valk(idim), nr=2, valr=valr)
            enddo
        enddo
        call utmess('A', 'MED_6')
    endif
!
9999  continue
!
    call jedetr('&&LRVCPG_COORD_PG_ASTER')
    call jedetr('&&LRVCPG_POIDS_PG_ASTER')
    call jedetr('&&LRVCPG_COORD_NO_MED')
    call jedetr('&&LRVCPG_COORD_PG_MED')
    call jedetr('&&LRVCPG_POIDS_PG_MED')
    call jedetr('&&LRVCPG_CORRESP_PG')
!
    call jedema()
!
    1001 format('  NOMBRE DE LOCALISATIONS LUES :',i4)
    1002 format('  LOCALISATION MED UTILISEE    :', a32)
    1100 format('  PT GAUSS',i4,' DIM ',i1,' COORD MED',1pe12.5,&
     &                                  ' COORD ASTER',1pe12.5)
end subroutine
