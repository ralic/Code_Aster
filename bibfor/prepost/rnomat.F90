subroutine rnomat(icesd, icesl, icesv, imap, nomcri,&
                  adrma, jtypma, k, optio, vala,&
                  valb, coefpa, nommat)
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
! person_in_charge: van-xuan.tran at edf.fr
    implicit      none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/rccome.h"
#include "asterfort/rcvale.h"
#include "asterfort/u2mess.h"
    integer :: icesd, icesl, icesv, imap, adrma, jtypma, k
    real(kind=8) :: vala, valb, coefpa
    character(len=8) :: nommat
    character(len=10) :: optio
    character(len=16) :: nomcri
! ----------------------------------------------------------------------
! BUT: RECUPERER POUR LA MAILLE COURANTE LE NOM DU MATERIAU DONNE PAR
!      L'UTILISATEUR.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  ICESD    IN   I  : ADRESSE DE L'OBJET CHAM_ELEM_S.CESD.
!  ICESL    IN   I  : ADRESSE DE L'OBJET CHAM_ELEM_S.CESL.
!  ICESV    IN   I  : ADRESSE DE L'OBJET CHAM_ELEM_S.CESV.
!  IMAP     IN   I  : NUMERO DE LA MAILLE COURANTE.
!  NOMCRI   IN   K  : NOM DU CRITERE.
!  ADRMA*   IN   I  : ADRESSE DE LA MAILLE CORRESPONDANT AU NOEUD.
!  JTYPMA*  IN   I  : ADRESSE DU TYPE DE LA MAILLE.
!  K*       IN   I  : POINTEUR SERVANT AU TEST : MATERIAU UNIQUE OU NON.
!  OPTIO    IN   K  : CALCUL AUX POINTS DE GAUSS OU AUX NOEUDS.
!  VALA     OUT  R  : VALEUR DU COEFFICIENT A.
!  VALB     OUT  R  : VALEUR DU COEFFICIENT B.
!  COEFPA   OUT  R  : COEFFICIENT DE PASSAGE CISAILLEMENT-TRACTION.
!  NOMMAT   OUT  K  : NOM DU MATERIAU AFFECTE A LA MAILLE COURANTE.
!
! REMARQUE : * ==> VARIABLES N'AYANT DE SENS QUE DANS LE CAS DU CALCUL
!                  DE LA FATIGUE AUX NOEUDS.
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: iad, numa, jtyp, ibid, iret
    real(kind=8) :: r8b
    integer :: icodre
    character(len=8) :: ktyp, dimk, k8b
    character(len=16) :: phenom
!     ------------------------------------------------------------------
!
!234567                                                              012
    k8b = '        '
    call jemarq()
!
    if (optio .eq. 'DOMA_ELGA') then
!
! 1. RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE COURANTE
!      _____________________________________________________
!     I                                                     I
!     I       CALCUL DE LA FATIGUE AUX POINTS DE GAUSS      I
!     I_____________________________________________________I
!
        call cesexi('C', icesd, icesl, imap, 1,&
                    1, 1, iad)
!C VERIFICATION HORS BORNES DEFINIES DANS CESMAT
!C OU COMPOSANTE NON AFFECTEE
        call assert(iad .gt. 0)
        nommat = zk8(icesv - 1 + iad)
!
    else if (optio .eq. 'DOMA_NOEUD') then
!
!      _____________________________________________________
!     I                                                     I
!     I           CALCUL DE LA FATIGUE AUX NOEUDS           I
!     I_____________________________________________________I
!
        numa = zi(adrma-1 + imap)
        jtyp = jtypma - 1 + numa
        call jenuno(jexnum( '&CATA.TM.NOMTM', zi(jtyp)), ktyp)
        call dismoi('F', 'TYPE_TYPMAIL', ktyp, 'TYPE_MAILLE', ibid,&
                    dimk, iret)
!
        if (dimk .eq. 'VOLU') then
            k = k + 1
            call cesexi('C', icesd, icesl, numa, 1,&
                        1, 1, iad)
!C VERIFICATION HORS BORNES DEFINIES DANS CESMAT
!C OU COMPOSANTE NON AFFECTEE
            call assert(iad .gt. 0)
            if ((k .gt. 1) .and. (nommat .ne. zk8(icesv - 1 + iad))) then
                call u2mess('F', 'FATIGUE1_33')
            else
                nommat = zk8(icesv - 1 + iad)
            endif
        endif
!
! CAS OU LES 1ERES MAILLES SCRUTEES NE SONT PAS VOLUMIQUE
! (ON PASSE A LA SUIVANTE)
        if (k .eq. 0) goto 999
!
    endif
!
! CAS CRITERE EST UNE FORMULE, ON NE RECUPERE QUE LE MATERIAU
    if (nomcri(1:7) .eq. 'FORMULE') then
        vala = 0.d0
        valb = 0.d0
        coefpa = 1.d0
        goto 999
    endif
!
    call rccome(nommat, 'CISA_PLAN_CRIT', phenom, icodre)
    if (icodre .eq. 1) then
        call u2mess('F', 'FATIGUE1_63')
    endif
!
! 2.1 RECUPERATION DES PARAMETRES ASSOCIES AU CRITERE MATAKE POUR
!     LA MAILLE COURANTE
!
    if (nomcri(1:14) .eq. 'MATAKE_MODI_AC') then
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'MATAKE_A', vala, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_64')
        endif
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'MATAKE_B', valb, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_65')
        endif
!
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'COEF_FLE', coefpa, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_66')
        endif
!
! 2.2 RECUPERATION DES PARAMETRES ASSOCIES AU CRITERE DE DANG VAN POUR
!     LA MAILLE COURANTE
!
    else if (nomcri(1:16) .eq. 'DANG_VAN_MODI_AC') then
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'D_VAN_A ', vala, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_67')
        endif
!
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'D_VAN_B ', valb, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_68')
        endif
!
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'COEF_CIS', coefpa, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_69')
        endif
    endif
!
! 2.3 RECUPERATION DES PARAMETRES ASSOCIES AU CRITERE MATAKE_MODI_AV
!     POUR LA MAILLE COURANTE
!
    if (nomcri(1:14) .eq. 'MATAKE_MODI_AV') then
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'MATAKE_A', vala, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_70')
        endif
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'MATAKE_B', valb, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_71')
        endif
!
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'COEF_FLE', coefpa, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_72')
        endif
    endif
!
! 2.4 RECUPERATION DES PARAMETRES ASSOCIES AU CRITERE DANG_VAN_MODI_AV
!     POUR LA MAILLE COURANTE
!
    if (nomcri(1:16) .eq. 'DANG_VAN_MODI_AV') then
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'D_VAN_A ', vala, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_73')
        endif
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'D_VAN_B ', valb, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_74')
        endif
!
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'COEF_CIS', coefpa, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_72')
        endif
    endif
!
! 2.5 RECUPERATION DES PARAMETRES ASSOCIES AU CRITERE FATEMI_SOCIE
!     POUR LA MAILLE COURANTE
!
    if (nomcri(1:16) .eq. 'FATESOCI_MODI_AV') then
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'FATSOC_A', vala, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_75')
        endif
!
        valb = 1.0d0
!
        call rcvale(nommat, 'CISA_PLAN_CRIT', 0, k8b, r8b,&
                    1, 'COEF_CIS', coefpa, icodre, 0)
        if (icodre .eq. 1) then
            call u2mess('F', 'FATIGUE1_72')
        endif
!
    endif
!
999  continue
!
    call jedema()
!
end subroutine
