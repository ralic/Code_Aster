subroutine ecrtes(nomsd, titre, nomgds, numor, fitype,&
                  nbcmp, ityp, entete, lcmp)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexpa.h"
#include "asterfort/rsnopa.h"
#include "asterfort/utmess.h"
    integer :: numor, ityp
    character(len=*) :: nomsd, titre, nomgds
    character(len=*) :: fitype
    character(len=80) :: entete(10)
    logical(kind=1) :: lcmp
!----------------------------------------------------------------------
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
!
!  ECRITURE DE L'EN TETE D'UN DATASET SUPERTAB
!  ENTREE:
!     NOMSD : NOM DU RESULTAT D'OU PROVIENT LE CHAMP A IMPRIMER
!     TITRE : TITRE SUPERTAB ( 1 LIGNE)
!     NOMGDS: NOM DE LA GRANDEUR DU CHAMP
!     NUMOR : NUMERO D'ORDRE DU CHAMP
!     FITYPE: 'NOEU' ===> DATASET DE TYPE 55
!             'ELGA' ===> DATASET DE TYPE 56 (MOYENNES PAR ELEMENT)
!             'ELEM' ===> DATASET DE TYPE 56 (VALEUR PAR ELEMENT)
!             'ELNO' ===> DATASET DE TYPE 57
!
!        CODES SUPERTAB:
!     MODTYP: 1  MECANIQUE, 2 THERMIQUE, 0 INCONNU
!     ANATYP: 1  STATIC, 2 MODAL, 4 TRANSITOIRE, 5 HARMONIQUE
!     DATCAR: 1  SCALAIRE, 3 6DOF,4 TENSEUR, 0 INCONNU
!     NUTYPE: 2  STRESS, 3 STRAIN, 4 ELEMENT FORCE, 5 TEMPERATURE
!             8  DEPLACEMENT, 11 VITESSE, 12 ACCELERATION, 0 INCONNU
!     LCMP  : PRECISE SI LE MOT CLE NOM_CMP DE IMPR_RESU EST PRESENT
!   SORTIE:
!     ENTETE:10 LIGNES D'EN-TETE DU DATASET SUPERTAB
!---------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: imode, itype, modtyp, anatyp, datcar, nutype
    integer :: nbtitr
    real(kind=8) :: freq, masgen, amor1, amor2, rvide
    character(len=8) :: k8bid
    character(len=16) :: typinc
    character(len=24) :: nomst
    character(len=80) :: stitr, stitrb
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iad, iret, istmax, itimax
    integer ::  jtitr, nbac, nbcmp, nbpa
    character(len=16), pointer :: nom_acc(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    itype = ityp
    rvide = r8vide()
!
    nomst= '&&IRECRI.SOUS_TITRE.TITR'
    call jeveuo(nomst, 'L', jtitr)
    stitr = zk80(jtitr)
    call jelira(nomst, 'LONMAX', nbtitr)
    if (nbtitr .ge. 2) then
        stitrb = zk80(jtitr+1)
    else
        stitrb = ' '
    endif
!
! ---CHOIX DU NUMERO DE DATASET--------------
    entete(1) = '    -1'
    if (fitype .eq. 'NOEU') then
        entete(2) = '    55   %VALEURS AUX NOEUDS'
    else if (fitype.eq.'ELGA') then
        entete(2) = '    56   %VALEURS MOYENNES PAR ELEMENT'
    else if (fitype.eq.'ELEM') then
        entete(2) = '    56   %VALEUR PAR ELEMENT'
    else if (fitype.eq.'ELNO') then
        entete(2) = '    57   %VALEURS AUX NOEUDS DES ELEMENTS'
    else
        ASSERT(.false.)
    endif
!
!   --- CHOIX DU TYPE DE MODELE--------
    if (nomgds .eq. 'TEMP' .or. nomgds .eq. 'FLUX') then
        modtyp = 2
    else
        modtyp = 1
    endif
!
!   --- A-T-ON UN CHAMP OU UN RESULTAT --------
    call dismoi('TYPE', nomsd, 'INCONNU', repk=typinc)
!
!   --- CHOIX DU TYPE D'ANALYSE--------
    anatyp = 0
    if (typinc .eq. 'CHAM_NO') then
        anatyp = 1
    else if (typinc .eq. 'CHAM_ELEM') then
        anatyp = 1
    else
        call rsnopa(nomsd, 0, '&&ECRTES.NOM_ACC', nbac, nbpa)
        call jeexin('&&ECRTES.NOM_ACC', iret)
        if (iret .gt. 0) call jeveuo('&&ECRTES.NOM_ACC', 'E', vk16=nom_acc)
        if (nbac .eq. 0) then
            anatyp = 1
        else
            do i = 1, nbac
                if (nom_acc(i) .eq. 'INST') then
                    anatyp = 4
                    goto 21
                else if (nom_acc(i).eq.'NUME_MODE') then
                    anatyp = 2
                    goto 21
                else if (nom_acc(i).eq.'FREQ') then
                    anatyp = 5
                endif
            end do
        endif
    endif
 21 continue
    call jedetr('&&ECRTES.NOM_ACC')
!
!   --- CHOIX DU TYPE DE CARACTERISTIQUES----
!   --- CHOIX DU TYPE DE RESULTAT ---
    if (nomgds .eq. 'DEPL') then
        datcar = 3
        nutype = 8
    else if (nomgds.eq.'VITE') then
        datcar = 3
        nutype = 11
    else if (nomgds.eq.'ACCE') then
        datcar = 3
        nutype = 12
    else if (nomgds.eq.'FLUX') then
        datcar = 2
        nutype = 6
    else if (nomgds.eq.'TEMP') then
        datcar = 1
        nutype = 5
    else if (nomgds.eq.'PRES') then
        datcar = 1
        nutype = 15
    else if (nomgds(1:3).eq.'SIG') then
        datcar = 4
        nutype = 2
    else if (nomgds.eq.'EPSI') then
        datcar = 4
        nutype = 3
    else
        datcar = 3
        nutype = 0
        if (nbcmp .eq. 1) datcar=1
    endif
    if (lcmp) then
        datcar = 3
    endif
    if (datcar .eq. 1) then
        nbcmp = 1
    else if (datcar.eq.2) then
        nbcmp = 3
    else if (datcar.eq.3.or.datcar.eq.4) then
        nbcmp = 6
    endif
!
    itimax = lxlgut(titre)
    istmax = lxlgut(stitr)
    istmax = min(istmax,36)
    itimax = min(itimax,72-istmax)
    entete(3)= titre(1:itimax)//' - '//stitr(1:istmax)
    entete(4) = ' '
    entete(5) = titre(1:80)
    entete(6) = stitr(1:80)
    entete(7) = stitrb(1:80)
    if (anatyp .eq. 0) then
        write (entete(8),1000) modtyp,anatyp,datcar,nutype ,itype,&
        nbcmp
        write (entete(9),2000) 1,1,numor
        write (entete(10),3000) 0.0d0
    else if (anatyp.eq.1) then
        write (entete(8),1000) modtyp,anatyp,datcar,nutype ,itype,&
        nbcmp
        write (entete(9),2000) 1,1,1
        write (entete(10),3000) 0.0d0
    else if (anatyp.eq.2) then
        call rsadpa(nomsd, 'L', 1, 'NUME_MODE', numor,&
                    0, sjv=iad, styp=k8bid)
        imode = zi(iad)
        call rsexpa(nomsd, 2, 'FREQ', iret)
        if (iret .ne. 0) then
            call rsadpa(nomsd, 'L', 1, 'FREQ', numor,&
                        0, sjv=iad, styp=k8bid)
        else
            call rsexpa(nomsd, 2, 'CHAR_CRIT', iret)
            if (iret .ne. 0) then
                call rsadpa(nomsd, 'L', 1, 'CHAR_CRIT', numor,&
                            0, sjv=iad, styp=k8bid)
            else
                k8bid = nomsd(1:8)
                call utmess('F', 'PREPOST_31', sk=k8bid)
            endif
        endif
        freq = zr(iad)
        call rsexpa(nomsd, 2, 'MASS_GENE', iret)
        if (iret .ne. 0) then
            call rsadpa(nomsd, 'L', 1, 'MASS_GENE', numor,&
                        0, sjv=iad, styp=k8bid)
            masgen= zr(iad)
        else
            masgen= 0.d0
        endif
!-MOD    CALL RSADPA(                  'AMORTISSEMENT VISQUEUX
!-MOD    CALL RSADPA(                  'AMORTISSEMENT STRUCTURAL
        call rsexpa(nomsd, 2, 'AMOR_REDUIT', iret)
        if (iret .ne. 0) then
            call rsadpa(nomsd, 'L', 1, 'AMOR_REDUIT', numor,&
                        0, sjv=iad, styp=k8bid)
            amor1 = zr(iad)
            if (amor1 .eq. rvide) amor1 = 0.0d0
        else
            amor1 = 0.0d0
        endif
        amor2 = 0.0d0
        write (entete(8),1000) modtyp,anatyp,datcar,nutype ,itype,&
        nbcmp
        write (entete(9),2000) 2,4,numor,imode
        write (entete(10),3000) freq,masgen,amor1,amor2
    else if (anatyp.eq.4) then
        call rsadpa(nomsd, 'L', 1, 'INST', numor,&
                    0, sjv=iad, styp=k8bid)
        write (entete(8),1000) modtyp,anatyp,datcar,nutype ,itype,&
        nbcmp
        write (entete(9),2000) 2,1,1,numor
        write (entete(10),3000) zr(iad)
    else if (anatyp.eq.5) then
        call rsadpa(nomsd, 'L', 1, 'FREQ', numor,&
                    0, sjv=iad, styp=k8bid)
        write (entete(8),1000) modtyp,anatyp,datcar,nutype ,itype,&
        nbcmp
        write (entete(9),2000) 2,1,1,numor
        write (entete(10),3000) zr(iad)
    endif
    1000 format(6i10)
    2000 format(8i10)
    3000 format(6(1pe13.5e3))
    call jedema()
end subroutine
