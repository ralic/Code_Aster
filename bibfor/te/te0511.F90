subroutine te0511(option, nomte)
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
! person_in_charge: alexandre.foucault at edf.fr
! =====================================================================
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/evala1.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
    character(len=16) :: option, nomte
! =====================================================================
!    - FONCTION REALISEE:  CALCUL DU MODULE DE RIGIDITE
!                          DE MICRO-DILTATION CONTENU
!                          AUX POINTS DE GAUSS
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! =====================================================================
! =====================================================================
    aster_logical :: logthm
    integer :: imate, icompo, ivarip, icontp, ilocal, ic, iv
    integer :: nbvari, rindic, kpg, nbsig, imat, ibid
    integer :: icode, iret, tabthm(3), dimmax, npgu
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
    real(kind=8) :: module, sig(6)
    real(kind=8) :: vin(100)
    character(len=8) :: mod, alias8
    character(len=16) :: relcom
! =====================================================================
! --- RINDIC EST LE NOMBRE DE PARAMETRE DE LOCALISATION DEFINIT -------
! --- SOUS LE MOT-CLE INDL_R DANS GRANDEUR_SIMPLE.CATA --------------
! =====================================================================
    parameter ( rindic  = 1 )
! =====================================================================
    call teattr('S', 'ALIAS8', alias8, ibid)
    if (option .eq. 'PDIL_ELGA') then
! =====================================================================
! --- VERIFICATION DE COHERENCE ---------------------------------------
! --- LE TENSEUR ACOUSTIQUE EST DEVELOPPE EN 2D UNIQUEMENT ------------
! =====================================================================
! --- CAS D'UN POST-TRAITEMENT EN MECANIQUE DRAINE --------------------
! =====================================================================
        logthm = .false.
        if ((alias8(3:5).eq.'DPL') .or. (alias8(3:5).eq.'DPS')) then
            mod(1:6) = 'D_PLAN'
            nbsig = nbsigm()
        else if (alias8(3:5).eq.'CPL') then
            mod(1:6) = 'C_PLAN'
            nbsig = nbsigm()
        else if (alias8(3:5).eq.'AX_') then
            mod(1:4) = 'AXIS'
            nbsig = nbsigm()
        else
! =====================================================================
! --- CAS D'UN POST-TRAITEMENT EN MECANIQUE THM -----------------------
! =====================================================================
            logthm = .true.
            if (alias8(3:5) .eq. 'AH2') then
                mod(1:4) = 'AXIS'
                else if ((alias8(3:5).eq.'DH2').or. (alias8(3:5).eq.'DR1')&
            .or. (alias8(3:5).eq.'DM1'))then
                mod(1:6) = 'D_PLAN'
            else
! =====================================================================
! --- CAS NON TRAITE --------------------------------------------------
! =====================================================================
                call utmess('F', 'ELEMENTS_11', sk=nomte)
            endif
        endif
! =====================================================================
! --- RECUPERATION DU ELREFE ------------------------------------------
! =====================================================================
        call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                         jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
! =====================================================================
! --- PARAMETRES EN ENTREE --------------------------------------------
! =====================================================================
        call jevech('PMATERC', 'L', imate)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PVARIPR', 'L', ivarip)
        if (logthm) then
! =====================================================================
! --- DANS LE CADRE THM ON FAIT UN TECACH PLUTOT QU'UN JEVECH POUR ----
! --- RECUPERER EGALEMENT LA DIMENSION DU VECTEUR QUI DIFFERE SUIVANT -
! --- LA MODELISATION THM ---------------------------------------------
! =====================================================================
            call tecach('OOO', 'PCONTPR', 'L', iret, nval=3,&
                        itab=tabthm)
            icontp = tabthm(1)
            dimmax = tabthm(2)
            npgu = tabthm(3)
! =====================================================================
! --- on teste la coherence des recuperations elrefe_info et tecach sur ----
! --- LE NOMBRE DE POINTS DE GAUSS ------------------------------------
! =====================================================================
            ASSERT(npgu.eq.npg)
            nbsig = dimmax / npg
! =====================================================================
! --- DANS LE CADRE DE LA THM ON RECUPERE DIRECTEMENT LA RELATION -----
! --- DE COMPORTEMENT DE TYPE MECANIQUE -------------------------------
! =====================================================================
            relcom = zk16(icompo-1+11)
        else
            call jevech('PCONTPR', 'L', icontp)
            relcom = zk16(icompo-1+ 1)
        endif
! =====================================================================
! --- NOMBRE DE VARIABLES INTERNES ASSOCIE A LA LOI DE COMPORTEMENT ---
! =====================================================================
        read (zk16(icompo-1+2),'(I16)') nbvari
! =====================================================================
! --- PARAMETRES EN SORTIE --------------------------------------------
! =====================================================================
        call jevech('PPDIL', 'E', ilocal)
! =====================================================================
! --- BOUCLE SUR LES POINTS DE GAUSS ----------------------------------
! =====================================================================
        do 10 kpg = 1, npg
! =====================================================================
! --- INITIALISATIONS -------------------------------------------------
! =====================================================================
            module = 0.0d0
! =====================================================================
! --- CALCUL DU MODULE DE RIGIDITE DE MICRO-DILTATION -----------------
! =====================================================================
            do 20 ic = 1, nbsig
                sig(ic) = zr(icontp-1+(kpg-1)*nbsig+ic )
 20         continue
            do 30 iv = 1, nbvari
                vin(iv) = zr(ivarip-1+(kpg-1)*nbvari+iv )
 30         continue
            imat = zi(imate)
            call evala1(mod, relcom, sig, vin, imat,&
                        module, icode)
! =====================================================================
! --- SURCHARGE DE L'INDICATEUR DE LOCALISATION -----------------------
! =====================================================================
            zr(ilocal-1+1+(kpg-1)*rindic) = module
 10     continue
    else
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
! =====================================================================
end subroutine
