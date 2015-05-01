subroutine materc(matmas, matrig, matamo, numnu, amor, nommes,&
                  lfreqs,nbfreq, matobs, obsdim, gamma, alpha,eval)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
! ----------------------------------------------------------------------
!
!  ROUTINE LIEE A L'OPERATEUR CALC_ERC_DYN
!
!  ON RECUPERE LES INFOS PRINCIPALES POUR LA RESOLUTION
!  DE L'ERC EN FREQUENTIEL
! ----------------------------------------------------------------------
! OUT : MATMAS  : NOM DE LA MATRICE DE MASSE
! OUT : MATRIG  : NOM DE LA MATRICE DE RIGIDITE
! OUT : MATAMO  : NOM DE LA MATRICE D'AMORTISSEMENT
! OUT : NUMNU   : NOM DU NUMEDDL DU MODELE M,C,K
! OUT : AMOR    : FLAG INDIQUANT LA PRESENCE D'AMORTISSMENT OU PAS 
! OUT : NOMMES  : NOM DU CONCEPT JEVEUX CONTENANT LA MESURE
! OUT : LFREQS  : NOM DU CONCEPT JEVEUX CONTENANT LES FREQUENCES DU CALCUL
! OUT : NBFREQS : NOMBRE DE FREQUENCES A CALCULER
! OUT : MATOBS  : LISTE DES NOMS DES OBJETS JEVEUX DEFINISSANT LA MATRICE
!                 D'OBSERVATION. LA MATRICE EST STOCKEE EN SPARSE SOUS LE
!                 FORMAT COO (FILE,COLONNE,VALEUR)
! OUT : OBSDIM  : TABLEAU DONNANT LES INFORMATIONS DIMENSIONNELLES DE LA
!                 MATRICE D'OBSERVATION (DIM_FILE,DIM_COLONNE,NOMBRE_DE_
!                 VALEURS_NONNULLES)
! OUT : GAMMA   : PARAMETRE GAMMA DE LA FONCTIONNELLE D'ERC
! OUT : ALPHA   : PARAMETRE ALPHA DE LA FONCTIONNELLE D'ERC
! OUT : EVAL    : FLAG INDIQUANT S'IL FAUT EVALUER LA FONCTIONNELLE D'ERC
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/isdeco.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/utmess.h"
!
    character(len=8),intent(out) :: matmas
    character(len=8),intent(out) :: matrig
    character(len=8),intent(out) :: matamo
    character(len=8),intent(out) :: numnu
    aster_logical,intent(out) :: amor
    character(len=8),intent(out) :: nommes
    character(len=24),intent(out) :: lfreqs
    integer,intent(out) :: nbfreq
    character(len=24),intent(out) :: matobs(3)
    integer,intent(out) :: obsdim(3)
    real(kind=8),intent(out) :: gamma, alpha
    aster_logical,intent(out) :: eval
    character(len=11) :: bl11
    character(len=3) :: rep_eval
    character(len=4) :: typmes
    character(len=8) :: baseno, k8bid, matprj, mainum, maiexp
    character(len=8) :: nomgd,  numdl1, numdl2, numdl3
    character(len=19) :: lifreq
    integer :: n1, lfreq,  iproj, nbnexp, inn, inp, nnopr, nec1, nec2
    integer :: idec(6), itach, tach1, iprnom, iprnoc, irefe, lprno, ipjnb
    integer :: ipjnu, ltest, nbddl, ieq, ihh, ipjcf, nbnonu, jj, inddl, icode(6), lnueqm, lnueqc
    integer :: iobfil, iobcol, iobval, ifreq, lh, cc, dd
!
! ----------------------------------------------------------------------
!
    baseno='&&OP0066'
    bl11 = '           '
! --- RECUPERATION DE LA MESURE
    call getvid(' ', 'MESURE', scal=nommes)
!
! --- RECUPERATION DES MATRICES
    call getvid(' ','MATR_PROJ',scal=matprj)
    call getvid(' ', 'MATR_MASS', scal=matmas)
    call getvid(' ', 'MATR_RIGI', scal=matrig)
!    call getvid(' ', 'MATR_AMOR', scal=matamo, nbret=n1)
    amor=.false._1
    matamo='        ' 
!    if (n1.eq.1) amor=.true._1
!
!   FAUT-IL EVALUER LA FONCTIONNELLE?
    call getvtx(' ', 'EVAL_FONC', scal=rep_eval)
    if (rep_eval.eq.'OUI') then
       eval=.true._1
    else
       eval=.false._1
    end if
!
! --- NUME_DDL DES MATRICES DU MODELE

    numdl1 = ' '
    numdl2 = ' '
    numdl3 = ' '

    call dismoi('NOM_NUME_DDL', matrig, 'MATR_ASSE', repk=numdl1)
    call dismoi('NOM_NUME_DDL', matmas, 'MATR_ASSE', repk=numdl2)
    if (amor) then
        call dismoi('NOM_NUME_DDL', matamo, 'MATR_ASSE', repk=numdl3)
    else
        numdl3 = numdl2
    endif
!
    if ((numdl1.ne.numdl2) .or. (numdl1.ne.numdl3) .or. (numdl2.ne.numdl3)) then
        call utmess('F', 'ALGORITH9_34')
    else
        numnu = numdl1
    endif
!
    call dismoi('NOM_GD', numnu, 'NUME_DDL', repk=nomgd)
    call jelira(numnu//'      .NUME.NUEQ', 'LONMAX', lnueqc, k8bid)
!
! --- ENTIERS CODES
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec1)
!
! --- ON CONSIDERE QUE LE NOMBRE D'ENTIERS CODES POUR LA MESURE EST 5
    nec2=5
!
! --- RECUPERATION DES MAILLAGES NUM/EXP ET CONCEPTS ASSOCIES
    call dismoi('NOM_MAILLA', matmas, 'MATR_ASSE', repk=mainum)
    call dismoi('NOM_MAILLA', nommes, 'RESULTAT', repk=maiexp)
!
! --- INFOS SUR LA MATRICE DE PROJECTION
    call jeveuo(matprj//'        .PJXX_K1', 'L', iproj)
    call jeveuo(matprj//'        .PJEF_NB', 'L', ipjnb)
    call jeveuo(matprj//'        .PJEF_NU', 'L', ipjnu)
    call jeveuo(matprj//'        .PJEF_CF', 'L', ipjcf)
!
! --- VERIFS DE COMPATIBILITE ENTRE LES MAILLAGES ET LA MATRICE DE PROJ
    if (zk24(iproj)(1:8).ne.mainum) then
       call utmess('F', 'ALGORITH9_66')
    else if (zk24(iproj+1)(1:8).ne.maiexp) then
       call utmess('F', 'ALGORITH9_66')
    else if (zk24(iproj+2)(1:11).ne.'COLLOCATION') then
       call utmess('F', 'ALGORITH9_66')
    endif
!
! --- ON VA CHERCHER LES INFOS DANS DE LA MESURE
    call getvtx(' ', 'CHAMP_MESURE', scal=typmes)
    call jenonu(jexnom(nommes//bl11//'.DESC', typmes), itach)
!
! --- A LA RECHERCHE DU PRNO
    call jeveuo(jexnum(nommes//bl11//'.TACH', itach), 'L', tach1)
    call jeveuo(zk24(tach1)(1:19)//'.REFE', 'L', irefe)

    if (zk24(irefe)(1:8).ne.maiexp) then
       call utmess('F', 'ALGORITH9_67')
    endif
    call jeveuo(jexnum(zk24(irefe+1)(1:19)//'.PRNO', 1), 'L', iprnom)
    call jelira(jexnum(zk24(irefe+1)(1:19)//'.PRNO', 1), 'LONMAX', lprno, k8bid)
    nbnexp=lprno/(2+nec2)
! --- ON VERIFIE L'ABSENCE DE LAGRANGES DANS LA MESURE
! --- ON ACCEPTE DES DDL PHYSIQUES (DX/DY/DZ/DRX/DRY/DRY) SEULEMENT
    call jelira(zk24(irefe+1)(1:19)//'.NUEQ', 'LONMAX', lnueqm, k8bid)
    dd=0
    lh=0
    do   jj = 1, nbnexp
        dd=dd+zi(iprnom+(jj-1)*(2+nec2)+1)
! --- CALCUL DES VALEURS NON NULLES DE LA MATRICE POUR LES WKVECT
        lh=lh+zi(iprnom+(jj-1)*(2+nec2)+1)*zi(ipjnb+jj-1)
    end do
    if (dd .ne. lnueqm) then
        call utmess('F', 'ALGORITH9_68')
    endif
! ----
    call jelira(matprj//'        .PJEF_NB', 'LONUTI', ltest, k8bid)
    if (ltest.ne.nbnexp) then
        call utmess('F', 'ALGORITH9_70')
    endif
    call jeveuo(numnu//'      .NUME.PRNO', 'L', iprnoc)
! --- IPRNOM pointe sur le .PRNO de la mesure
! --- IPRNOC pointe sur le .PRNO du modele numerique
!
! --- CONSTRUCTION DE LA MATRICE D'OBSERVATION POUR L'ERC
!
! --- CREATION DES VECTEURS DE TRAVAIL
    matobs(1)=baseno//'.MAT.OBS.ERC.FIL'
    matobs(2)=baseno//'.MAT.OBS.ERC.COL'
    matobs(3)=baseno//'.MAT.OBS.ERC.VAL'
    call wkvect(matobs(1), 'V V I', lh, iobfil)
    call wkvect(matobs(2), 'V V I', lh, iobcol)
    call wkvect(matobs(3), 'V V R', lh, iobval)
!
! --- ON DONNE LES INFOS DIMENSIONNELLES DE LA MATRICE D'OBS EN SPARSE
    obsdim(1)=lnueqm
    obsdim(2)=lnueqc
    obsdim(3)=lh
!
!     BOUCLE SUR LES NOEUDS EXP
    ihh=0
    do   inn = 1, nbnexp
        ieq=zi(iprnom+(inn-1)*(2+nec2))
        nbddl=zi(iprnom+(inn-1)*(2+nec2)+1)
        call isdeco(zi(iprnom+(inn-1)*(2+nec2)+2), idec, 6)
        nnopr=zi(ipjnb+inn-1)
!     ON DECODE LES COMPOSANTES DE CHAQUE DDL
!     PORTES PAR LE NOEUD EN COURS: (DX/DY/DZ/DRX/DRY/DRY) ONLY!!
        cc=1
        do   jj = 1, 6
            icode(jj)=0
            if (idec(jj) .eq. 1) then
                icode(cc)=jj
                cc=cc+1
            endif
        end do
!      BOUCLE SUR CHAQUE COMPOSANTE
!      ( ON SUPPOSE INITIALEMENT DX/DY/DZ/DRX/DRY/DRY )
        do   inddl = 1, nbddl
!      BOUCLE SUR LES NOEUDS QUI PARTICIPENT A L'OBSERVATION
            do   inp = 1, nnopr
                nbnonu=zi(ipjnu+(inn-1)*nnopr+inp-1)
                zi(iobfil+ihh)=ieq+inddl-1
                zi(iobcol+ihh)=zi(iprnoc+(nbnonu-1)*(2+nec1))+icode(&
                inddl)-1
                zr(iobval+ihh)=zr(ipjcf+(inn-1)*nnopr+inp-1)
                ihh=ihh+1
            end do
        end do
    end do
!
    if (lh.ne.ihh) then
        call utmess('F', 'ALGORITH9_71')
    endif
!
! --- LISTE DES FREQUENCES POUR LE CALCUL
    call getvid(' ', 'LIST_FREQ', scal=lifreq, nbret=n1)
    if (n1 .gt. 0) then
        call jeveuo(lifreq//'.VALE', 'L', lfreq)
        call jelira(lifreq//'.VALE', 'LONMAX', nbfreq, k8bid)
    else

        call getvr8(' ', 'FREQ', nbval=0, nbret=nbfreq)
        nbfreq = - nbfreq
        call wkvect(baseno//'.LISTE.FREQ', 'V V R', nbfreq, lfreq)
        call getvr8(' ', 'FREQ', nbval=nbfreq, vect=zr(lfreq))

    endif
    lfreqs=baseno//'.LISTE.FREQS.ERC'
    call wkvect(lfreqs, 'V V R', nbfreq, ifreq)
    call dcopy(nbfreq, zr(lfreq), 1, zr(ifreq), 1)
!
! --- COEFFICIENTS POUR L'ERC
      call getvr8(' ', 'GAMMA', nbval=1, scal=gamma)
      call getvr8(' ', 'ALPHA', nbval=1, scal=alpha)

end subroutine
