subroutine op0199()
    implicit none
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
! OPERATEUR CALCULANT LA FORCE AJOUTEE : CALC_FORC_AJOU
!
!---------------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/cal152.h"
#include "asterfort/calmdg.h"
#include "asterfort/cresol.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mat152.h"
#include "asterfort/phi199.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rigflu.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/wkvect.h"
    integer :: ibid, nbmo, nbmode(1), ndble, indice, ifm, niv
    integer :: tabad(5), iadesc, iarefe, i, iadirg, imade
    integer :: iphi1, iphi2, iprsto, iret, itxsto
    integer :: itysto, itzsto, ivalk, ivale
    integer :: n1, n2, n3, n4, n5, n6, n7, n8, n9
    real(kind=8) :: rbid, tps(6), mij, cij, kij
    complex(kind=8) :: cbid
    aster_logical :: vrai
    character(len=2) :: model
    character(len=3) :: nd
    character(len=8) :: nomres, k8bid, modmec, phibar, moint, char
    character(len=8) :: moflui, ma, materi, nomcmp(6), numgen, modgen
    character(len=14) :: nu, num, nugene
    character(len=16) :: typres, nomcom
    character(len=19) :: max, may, maz, chamno, solveu
    character(len=24) :: blanc, time, nocham, mate
!
! -----------------------------------------------------------------
    data nomcmp / 'INST    ', 'DELTAT  ', 'THETA   ',&
     &               'KHI     ', 'R       ', 'RHO     ' /
    data tps    / 0.0d0, 2*1.0d0, 3*0.0d0 /
    data solveu / '&&OP0199.SOLVEUR' /
!
!-----------------------------------------------------------------
!
    call jemarq()
!
    call getres(nomres, typres, nomcom)
!
    call infmaj()
    call infniv(ifm, niv)
!
    nbmo = 0
    ndble = 0
    vrai = .true.
    time = '&TIME'
    nugene = ' '
    materi = ' '
    mate = ' '
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getvid(' ', 'MODELE_FLUIDE', scal=moflui, nbret=n1)
    call getvid(' ', 'CHARGE', scal=char, nbret=n2)
    call getvid(' ', 'MODELE_INTERFACE', scal=moint, nbret=n3)
    call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n4)
    call getvid(' ', 'MODE_MECA', scal=modmec, nbret=n5)
    call getvid(' ', 'NUME_DDL_GENE', scal=numgen, nbret=n6)
    call getvid(' ', 'MODELE_GENE', scal=modgen, nbret=n7)
    call getvid(' ', 'POTENTIEL', scal=phibar, nbret=n8)
    call getvtx(' ', 'NOEUD_DOUBLE', scal=nd, nbret=n9)
!
! --- LECTURE DES PARAMETRES  SOLVEUR
!
    call cresol(solveu)
!
    if (n4 .ne. 0) call rcmfmc(materi, mate)
!
    if (n6 .ne. 0) nugene = numgen
!
    if (n5 .ne. 0) then
        call rsorac(modmec, 'LONUTI', 0, rbid, k8bid,&
                    cbid, rbid, 'ABSOLU', nbmode, 1,&
                    ibid)
        nbmo = nbmode(1)
        call rsexch(' ', modmec, 'DEPL', 1, nocham,&
                    iret)
    endif
!
    if (n7 .ne. 0) then
        if (nd .eq. 'OUI') ndble = 1
    endif
!
    model = '  '
!
!--------------------------------------------------------------
! --- CALCUL DE LA MATRICE ASSEMBLEE DE RIGIDITE DU FLUIDE
!--------------------------------------------------------------
!
    call rigflu(moflui, time, nomcmp, tps, n2,&
                char, mate, solveu, ma, nu)
!
!--------------------------------------------------------------
! CALCUL DES MATR_ELEM AX ET AY DANS L'OPTION FLUX_FLUI_X ET _Y
!---------------SUR LE MODELE INTERFACE(THERMIQUE)-------------
! CALCUL DES MATRICES MODALES BI POUR L OPTION AMOR_AJOU
!--------------------------------------------------------------
!
    call mat152('MASS_AJOU', model, moint, nocham, ivalk,&
                nbmo, max, may, maz, num)
!
    call jeexin('&&MAT152.MADE', iret)
    if (iret .gt. 0) call jeveuo('&&MAT152.MADE', 'E', imade)
!
!================================================================
! CALCUL ET STOCKAGE DES POTENTIELS INSTATIONNAIRES PHI1 ET PHI2
! CORRESPONDANT RESPECTIVEMENT AUX EFFETS INERTIELS
! ET AUX EFFETS D'AMORTISSEMENT ET DE RAIDEUR DU FLUIDE
! SUR LA STRUCTURE
!================================================================
!
    call phi199(model, mate, ma, nu, num,&
                nbmo, solveu, indice, tabad)
!
!--------------------------------------------------------------
! VERIFICATION D EXISTENCE DE VECTEUR DE CHAMPS AUX NOEUDS CREES
! DS PHI152 ILS SERONT ENSUITE EXPLOITES DS CAL152 ENTRE AUTRES
! VECTEUR DE NOMS DU POTENTIEL INSTATIONNAIRE PHI1 : MASSE AJOU
! ON Y STOCKE LES NOMS DES POTENTIELS INSTATIONNAIRES POUR
! CHAQUE MODE DE STRUCTURE
!
    call jeexin('&&OP0199.PHI1', iret)
    if (iret .gt. 0) call jeveuo('&&OP0199.PHI1', 'E', iphi1)
    call jeexin('&&OP0199.PHI2', iret)
    if (iret .gt. 0) call jeveuo('&&OP0199.PHI2', 'E', iphi2)
!
!=====================================================================
!---------------------------------------------------------------------
!              CALCUL SUR MODELE GENERALISE
!---------------------------------------------------------------------
!=====================================================================
!
    if (n7 .gt. 0) then
        call calmdg(model, modgen, nugene, num, nu,&
                    ma, mate, moint, moflui, ndble,&
                    itxsto, itysto, itzsto, iprsto, nbmo,&
                    iadirg)
    endif
!
!=============================================================
!--------REMPLISSAGE DU  .VALE : CALCUL DU VECTEUR AJOUTE
!=============================================================
!
!---------------------------------------------------------------
    if ((n7.gt.0) .or. (indice.eq.1)) then
!
! CALCUL DU VECTEUR AJOUTE - PRODUITS SCALAIRES SUR MODELE
! GENERALISE - CAS DE LA SOUS-STRUCTURATION DYNAMIQUE
! OU BIEN CAS DE MODES RESTITUES SUR MAILLAGE SQUELETTE
!
        if (indice .eq. 1) then
            itxsto = tabad(1)
            itysto = tabad(2)
            itzsto = tabad(3)
            iprsto = tabad(4)
            iadirg = tabad(5)
            nbmo=nbmode(1)
        endif
    else
!
! --- CREATION DE L OBJET VECT_GENE RESULTAT
!
        call wkvect(nomres//'           .VALE', 'G V R', nbmo, ivale)
        call wkvect(nomres//'           .REFE', 'G V K24', 2, iarefe)
        call wkvect(nomres//'           .DESC', 'G V I', 3, iadesc)
        call jeecra(nomres//'           .DESC', 'DOCU', cval='VGEN')
!
! --- REMPLISSAGE DU .REFE ET .VALE
!
        zk24(iarefe) = modmec
        zk24(iarefe+1) = nugene
        zi(iadesc) = 1
        zi(iadesc+1) = nbmo
!
        do 10 i = 1, nbmo
!
            blanc = ' '
            call cal152('MASS_AJOU', max, may, maz, model,&
                        blanc, iphi1, iphi2, imade, modmec,&
                        chamno, num, vrai, i, 1,&
                        mij, cij, kij)
!
            zr(ivale+i-1) = mij
!
 10     continue
    endif
!
!
    call jedetc('G', '&&RIGFLU', 1)
!
    call jedema()
end subroutine
