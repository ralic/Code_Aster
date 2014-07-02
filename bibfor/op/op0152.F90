subroutine op0152()
    implicit none
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
! AUTEUR : G.ROUSSEAU
! OPERATEUR CALCULANT LA MASSE AJOUTEE, L'AMORTISSEMENT
!  ET LA RIGIDITE AJOUTEE EN THEORIE POTENTIELLE : CALC_MATR_AJOU
!  SUR BASE MODALE DE LA STRUCTURE DANS LE VIDE
!---------------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/cal152.h"
#include "asterfort/calmdg.h"
#include "asterfort/chpver.h"
#include "asterfort/cresol.h"
#include "asterfort/crnslv.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mag152.h"
#include "asterfort/mamodg.h"
#include "asterfort/mat152.h"
#include "asterfort/phi152.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rigflu.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/ualfva.h"
#include "asterfort/ver152.h"
#include "asterfort/wkvect.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
!
    aster_logical :: vrai
    integer :: ldblo, ibid
    integer :: nbmo, nbmode, ndble, indice, tabad(5)
    integer :: i, j, jdesc
    integer :: iadirg, iblo, ierd
    integer :: imade
    integer :: iphi1, iphi2, iprsto, iret, itxsto
    integer :: itysto, itzsto, ivalk, jscbl
    integer :: jscdi, jscde, jschc, jscib, n1bloc, n2bloc
    integer :: nbid, nbloc, nterm
    integer :: n1, n2, n3, n4, n5, n6, n7, n9, n10, n12, n13, n14
    integer :: ifm, niv, tmod(1)
    real(kind=8) :: tps(6), mij, cij, kij
    real(kind=8) :: bid, ebid
    character(len=2) :: model
    character(len=3) :: nd
    character(len=8) :: nomres, k8bid, modmec, phibar
    character(len=8) :: moflui, moint, ma, materi, nomcmp(6)
    character(len=8) :: char, numgen, modgen
    character(len=9) :: option
    character(len=14) :: nu, num, nugene
    character(len=16) :: typres, nomcom
    character(len=19) :: max, may, maz, chamno
    character(len=19) :: stolci, solveu, nomr19
    character(len=24) :: nomcha, time, nocham
    character(len=24) :: mate, phib24
    complex(kind=8) :: cbid
! -----------------------------------------------------------------
    data nomcmp /'INST    ','DELTAT  ','THETA   ',&
     &             'KHI     ','R       ','RHO     '/
    data tps    /0.0d0,2*1.0d0,3*0.0d0/
    data solveu   /'&&OP0152.SOLVEUR'/
!
!-----------------------------------------------------------------
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    time = '&TIME'
    vrai = .true.
!
    call getres(nomres, typres, nomcom)
    nomr19=nomres
!
!----------RECUPERATION DES ARGUMENTS DE LA COMMANDE--------------
!
!
    materi = ' '
    call getvid(' ', 'MODELE_FLUIDE', scal=moflui, nbret=n1)
    call getvid(' ', 'CHARGE', scal=char, nbret=n2)
    call getvid(' ', 'MODELE_INTERFACE', scal=moint, nbret=n3)
    call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n4)
    modmec=' '
    call getvid(' ', 'MODE_MECA', scal=modmec, nbret=n5)
    call getvid(' ', 'CHAM_NO', nbval=0, nbret=n6)
    call getvid(' ', 'NUME_DDL_GENE', scal=numgen, nbret=n9)
    call getvid(' ', 'MODELE_GENE', scal=modgen, nbret=n10)
    call getvid(' ', 'POTENTIEL', scal=phibar, nbret=n12)
    call getvtx(' ', 'OPTION', scal=option, nbret=n13)
    call getvtx(' ', 'NOEUD_DOUBLE', scal=nd, nbret=n14)
!
! LECTURE DES PARAMETRES DONNES APRES LE MOT CLE FACTEUR SOLVEUR
!
    call cresol(solveu)
!
! VERIFICATIONS SUPPLEMENTAIRES
!
    call ver152(option, moflui, moint, n12, model)
!
! EXTRACTION DU POTENTIEL PERMANENT DES VITESSES
!
    if (option .eq. 'AMOR_AJOU' .or. option .eq. 'RIGI_AJOU') then
        call rsexch(' ', phibar, 'TEMP', 0, phib24,&
                    iret)
    endif
!
!
!     CAS NUME_DDL_GENE PRESENT
!     ------------------------------------
    if (n9 .ne. 0) then
        nugene = numgen
        stolci = nugene//'.SLCS'
!
    else
!       CREATION D UN NUME_DDL_GENE
!       ------------------------------------
        nugene = nomres
        stolci = nugene//'.SLCS'
        call crnslv(nugene, 'LDLT', 'SANS', 'G')
!
        nbmode = -n6
        call wkvect(stolci//'.SCHC', 'G V I', nbmode, jschc)
        do 200 i = 1, nbmode
            zi(jschc+i-1)=i
200     continue
!
        nbloc=1
        call wkvect(stolci//'.SCIB', 'G V I', nbmode, jscib)
        do 110 i = 1, nbmode
            zi(jscib+i-1)=nbloc
110     continue
!
        call wkvect(stolci//'.SCBL', 'G V I', nbloc+1, jscbl)
        zi(jscbl)=0
        zi(jscbl+1)=nbmode
!
        call wkvect(stolci//'.SCDI', 'G V I', nbmode, jscdi)
        nterm=0
        do 120 i = 1, nbmode
            nterm=nterm+zi(jschc+i-1)
            zi(jscdi+i-1)=nterm
120     continue
!
        call wkvect(stolci//'.SCDE', 'G V I', 6, jscde)
        zi(jscde-1+1)=nbmode
        zi(jscde-1+2)=nterm
        zi(jscde-1+3)=nbloc
        zi(jscde-1+4)=nbmode
    endif
!
!
    if (n6 .ne. 0) then
        n7 = -n6
        vrai=.false.
    else
        n7=0
    endif
!
!--------- RECUPERATION DU MATERIAU FLUIDE----------------------------
    if (n4 .ne. 0) then
        call rcmfmc(materi, mate)
    else
        mate = ' '
    endif
!
!--------CALCUL DE LA MATRICE ASSEMBLEE DE RIGIDITE DU FLUIDE---------
!
    call rigflu(moflui, time, nomcmp, tps, n2,&
                char, mate, solveu, ma, nu)
!
!=====================================================================
!---------------- ALTERNATIVE CHAMNO OU MODE_MECA OU---------
!-----------------------------MODELE-GENE--------------------
!=====================================================================
!
!----------------------------------------------------------------
    if (n5 .gt. 0) then
        call rsorac(modmec, 'LONUTI', ibid, bid, k8bid,&
                    cbid, ebid, 'ABSOLU', tmod, 1,&
                    nbid)
        nbmode=tmod(1)
        nbmo = nbmode
        call rsexch(' ', modmec, 'DEPL', 1, nomcha,&
                    iret)
        nocham = nomcha
    else
        if (n7 .gt. 0) then
            nbmo = n7
! 1ERE CREATION DE VECTEUR DE NOMS DES CHAMPS DE DEPL_R
! REPRESENTANT LES MODES
! EN CAS D'UTILISATION DU MOT CLE CHAM-NO, CECI POUR
! MAT152
            call jecreo('&&OP0152.VEC', 'V V K8')
            call jeecra('&&OP0152.VEC', 'LONMAX', n7)
            call jeveuo('&&OP0152.VEC', 'E', ivalk)
            call getvid(' ', 'CHAM_NO', nbval=n7, vect=zk8(ivalk), nbret=n6)
            nocham = zk8(ivalk)
            call chpver('F', nocham, 'NOEU', 'DEPL_R', ierd)
        endif
    endif
!--------------------------------------------------------------
! CALCUL DES MATR_ELEM AX ET AY DANS L'OPTION FLUX_FLUI_X ET _Y
!---------------SUR LE MODELE INTERFACE(THERMIQUE)-------------
! CALCUL DES MATRICES MODALES BI POUR L OPTION AMOR_AJOU
!--------------------------------------------------------------
    call mat152(option, model, moint, nocham, ivalk,&
                nbmo, max, may, maz, num)
    call jeexin('&&MAT152.MADE', iret)
    if (iret .gt. 0) call jeveuo('&&MAT152.MADE', 'E', imade)
! DESTRUCTION DU VECTEUR DE NOMS DES DEPL-R POUR RECREATION DS
! PHI152
    call jeexin('&&OP0152.VEC', iret)
    if (iret .gt. 0) call jedetr('&&OP0152.VEC')
!
!================================================================
! CALCUL ET STOCKAGE DES POTENTIELS INSTATIONNAIRES PHI1 ET PHI2
! CORRESPONDANT RESPECTIVEMENT AUX EFFETS INERTIELS
! ET AUX EFFETS D'AMORTISSEMENT ET DE RAIDEUR DU FLUIDE
! SUR LA STRUCTURE
!================================================================
    call phi152(model, option, mate, phib24, ma,&
                nu, num, nbmode, solveu, indice,&
                tabad)
!
! VERIFICATION D EXISTENCE DE VECTEUR DE CHAMPS AUX NOEUDS CREES
! DS PHI152 ILS SERONT ENSUITE EXPLOITES DS CAL152 ENTRE AUTRES
! VECTEUR DE NOMS DU POTENTIEL INSTATIONNAIRE PHI1 : MASSE AJOU
! ON Y STOCKE LES NOMS DES POTENTIELS INSTATIONNAIRES POUR
! CHAQUE MODE DE STRUCTURE
    call jeexin('&&OP0152.PHI1', iret)
    if (iret .gt. 0) call jeveuo('&&OP0152.PHI1', 'E', iphi1)
!
! VECTEUR DE NOMS DU POTENTIEL INSTATIONNAIRE PHI2 : AMOR AJOU
! RIGI AJOU
    call jeexin('&&OP0152.PHI2', iret)
    if (iret .gt. 0) call jeveuo('&&OP0152.PHI2', 'E', iphi2)
!
! VECTEUR DE NOMS DES CHAMPS DE DEPL_R REPRESENTANT LES MODES
! EN CAS D'UTILISATION DU MOT CLE CHAM-NO
    call jeexin('&&OP0152.VEC', iret)
    if (iret .gt. 0) call jeveuo('&&OP0152.VEC', 'E', ivalk)
!
!
!
!================================================================
!----------- CREATION DE LA MATR_ASSE_GENE    -------------------
!----------- CONTENANT LA MASSE AJOUTEE RESULTAT   --------------
!================================================================
!
    call mag152(n9, n10, nomres, nugene, modmec,&
                modgen, nbloc, indice)
!
!=====================================================================
!---------------------------------------------------------------------
!              CALCUL SUR MODELE GENERALISE
!---------------------------------------------------------------------
!=====================================================================
!
    if (n10 .gt. 0) then
        if (nd .eq. 'OUI') then
            ndble=1
        else
            ndble=0
        endif
        call calmdg(model, modgen, nugene, num, nu,&
                    ma, mate, moint, moflui, ndble,&
                    itxsto, itysto, itzsto, iprsto, nbmo,&
                    iadirg)
!
    endif
!
!
!=============================================================
!--------REMPLISSAGE DU  .VALE : CALCUL DE LA MASSE AJOUTEE
!=============================================================
!
!---------------------IMPRESSION DES RESULTATS------------------
!
    if (niv .gt. 1) then
        if (option .eq. 'MASS_AJOU') then
            write(ifm,*) '        '
            write(ifm,*) '          =======MATRICE DE MASSE AJOUTEE======='
            if (n10 .gt. 0) then
                write(ifm,*) '           ========HORS DDL DE LAGRANGE==='
            endif
        endif
        if (option .eq. 'AMOR_AJOU') then
            write(ifm,*) '        '
            write(ifm,*) '         =====MATRICE D AMORTISSEMENT AJOUTE====='
        endif
        if (option .eq. 'RIGI_AJOU') then
            write(ifm,*) '        '
            write(ifm,*) '        =======MATRICE DE RIGIDITE AJOUTEE======='
        endif
    endif
!---------------------------------------------------------------
    if ((n10.gt.0) .or. (indice.eq.1)) then
!
! CALCUL DES MASSES AJOUTEES - PRODUITS SCALAIRES SUR MODELE
! GENERALISE - CAS DE LA SOUS-STRUCTURATION DYNAMIQUE
! OU BIEN CAS DE MODES RESTITUES SUR MAILLAGE SQUELETTE
!
        if (indice .eq. 1) then
            itxsto = tabad(1)
            itysto = tabad(2)
            itzsto = tabad(3)
            iprsto = tabad(4)
            iadirg = tabad(5)
            nbmo=nbmode
        endif
!
        call mamodg(model, stolci, nomres, itxsto, itysto,&
                    itzsto, iprsto, iadirg, nbmo, max,&
                    may, maz, nbloc)
    else
!
! CAS CLASSIQUE
!
        call jeveuo(stolci//'.SCDI', 'L', jscdi)
        call jeveuo(stolci//'.SCBL', 'L', jscbl)
        call jeveuo(stolci//'.SCHC', 'L', jschc)
        call jeveuo(stolci//'.SCIB', 'L', jscib)
!
!     BOUCLE SUR LES BLOCS DE LA MATRICE ASSEMBLEE GENE
!
        do 40 iblo = 1, nbloc
            call jecroc(jexnum(nomr19//'.UALF', iblo))
            call jeveuo(jexnum(nomr19//'.UALF', iblo), 'E', ldblo)
!----------------------------------------------------------------
!
!         BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
            n1bloc=zi(jscbl+iblo-1)+1
            n2bloc=zi(jscbl+iblo)
!
!
            do 10 i = n1bloc, n2bloc
                do 30 j = (i-zi(jschc+i-1)+1), i
!
!----------------------------------------------------------------
! ICI ON CALCULE LA MASSE AJOUTEE SUR UN MODELE GENERALISE
!----------------------------------------------------------------
!
!------------------------------------------------------------------
! ICI ON CALCULE LA MATRICE DE MASSE AJOUTEE SOIT SUR UN MODE_MECA
! SOIT SUR UN CHAM_NO
!------------------------------------------------------------------
!
                    if (n7 .gt. 0) then
                        chamno=zk8(ivalk+i-1)
                    endif
!
                    call cal152(option, max, may, maz, model,&
                                phib24, iphi1, iphi2, imade, modmec,&
                                chamno, num, vrai, i, j,&
                                mij, cij, kij)
!
!
!
!-----------STOCKAGE DANS LA MATR_ASSE_GENE  ------
!
!        CAS DE LA PROJECTION MODALE OU CHAM_NO
!
!
                    if (option .eq. 'MASS_AJOU') then
                        zr(ldblo+zi(jscdi+i-1)+j-i-1) = mij
                    endif
                    if (option .eq. 'AMOR_AJOU') then
                        zr(ldblo+zi(jscdi+i-1)+j-i-1) = cij
                    endif
                    if (option .eq. 'RIGI_AJOU') then
                        zr(ldblo+zi(jscdi+i-1)+j-i-1) = kij
                    endif
!
!===============================================================
!---------------IMPRESSION DES RESULTATS------------------------
!===============================================================
!
                    if (niv .gt. 1) then
                        if (((n9.gt.0).and.(n5.ne.0)) .or. (n6.ne.0)) then
                            if (option .eq. 'MASS_AJOU') then
                                write(ifm,350) i,j,zr(ldblo+j+(i-1)*i/&
  2                             -1)
                            endif
                            if (option .eq. 'AMOR_AJOU') then
                                write(ifm,450) i,j,zr(ldblo+j+(i-1)*i/&
  2                             -1)
                            endif
                            if (option .eq. 'RIGI_AJOU') then
                                write(ifm,550) i,j,zr(ldblo+j+(i-1)*i/&
  2                             -1)
                            endif
                            350 format(18x,'M',2 i 4,1x,'=',1x, d 12.5)
                            450 format(18x,'C',2 i 4,1x,'=',1x, d 12.5)
                            550 format(18x,'K',2 i 4,1x,'=',1x, d 12.5)
                        endif
                    endif
 30             continue
 10         continue
 40     continue
    endif
!
    if (niv .gt. 1) then
!
        write(ifm,*) '              ============================'
        write(ifm,*) '              =======FIN IMPRESSION======='
        write(ifm,*) '              ============================'
!
    endif
!
!
!   -- on repasse au stockage morse qui est le stockage normal :
!   ------------------------------------------------------------
    call ualfva(nomres, 'G')
!
!   -- on corrige l'objet .DESC :
!   ------------------------------------------------------------------------------
    call jelira(nomr19//'.CONL', 'LONMAX', n1)
    call jelira(jexnum(nomr19//'.VALM', 1), 'LONMAX', n2)
    call jeveuo(nomr19//'.DESC', 'E', jdesc)
    zi(jdesc)=2
    zi(jdesc+1)=n1
    if (n2 .eq. n1) then
        zi(jdesc+2)=1
    else if (n2.eq.n1*(n1+1)/2) then
        zi(jdesc+2)=2
    else
        zi(jdesc+2)=3
    endif
!
!
    call jedetc('G', '&&RIGFLU', 1)
    call jedetc('G', '&&CALMAA', 1)
!
    call jedema()
end subroutine
