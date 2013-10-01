subroutine caelca(modele, chmat, caelem, irana1, icabl,&
                  nbnoca, numaca, quad, regl, relax,&
                  ea, rh1000, prelax, fprg, frco,&
                  frli, sa)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  DESCRIPTION : RECUPERATION DES CARACTlERISTIQUES ELEMENTAIRES
!  -----------   D'UN CABLE
!                CES DONNEES PEUVENT ETRE DEFINI PAR BPEL_ACIER
!                                                OU ETCC_ACIER
!                APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!  IN     : MODELE : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MODELE ASSOCIE A L'ETUDE
!  IN     : CHMAT  : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT CHAM_MATER ASSOCIE A L'ETUDE
!  IN     : CAELEM : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT CARA_ELEM ASSOCIE A L'ETUDE
!  IN     : IRANA1 : INTEGER , SCALAIRE
!                    RANG DE LA COMPOSANTE <A1> DE LA GRANDEUR <CAGNBA>
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  IN     : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
!                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
!  IN     : NUMACA : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DES MAILLES APPARTENANT AUX CABLES
!                    CE VECTEUR EST COMPLETE LORS DU PASSAGE PREALABLE
!                    DANS LA ROUTINE TOPOCA
! IN      : QUAD   : VRAI SI MAILLAGE QUADRATIQUE (SEG3)
!
! IN      : REGL    : CHARACTER*4, NOM DU REGLEMENT UTILISE
!                     BPEL OU ETCC
!  IN/OUT : RELAX  : LOGICAL , SCALAIRE
!                    INDICATEUR DE PRISE EN COMPTE DES PERTES DE TENSION
!                    PAR RELAXATION DE L'ACIER
!                    SI RH1000 = 0  => RELAX = .FALSE.
!  OUT    : EA     : REAL*8 , SCALAIRE
!                    VALEUR DU MODULE D'YOUNG DE L'ACIER
!  OUT    : RH1000 : REAL*8 , SCALAIRE
!                    VALEUR DE LA RELAXATION A 1000 HEURES EN %
!  OUT    : PRELAX    : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE RELAXATION DE L'ACIER
!                    PRECONTRAINT
!  OUT    : FPRG  : REAL*8 , SCALAIRE
!                    VALEUR DE LA CONTRAINTE LIMITE ELASTIQUE DE L'ACIER
!  OUT    : FRCO   : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE FROTTEMENT EN COURBE
!                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
!  OUT    : FRLI   : REAL*8 , SCALAIRE
!                    VALEUR DU COEFFICIENT DE FROTTEMENT EN LIGNE
!                    (CONTACT ENTRE LE CABLE ACIER ET LE MASSIF BETON)
!  OUT    : SA     : REAL*8 , SCALAIRE
!                    VALEUR DE L'AIRE DE LA SECTION DROITE DU CABLE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: modele, chmat, caelem
    integer :: irana1, icabl, nbnoca(*)
    character(len=19) :: numaca
    logical :: relax, quad
    real(kind=8) :: ea, rh1000, prelax, fprg, frco, frli, sa
!
! VARIABLES LOCALES
! -----------------
    integer :: ias, iasmax, icmp, icode, icste, idecma, imail, iranv, iret
    integer :: jdesc, jmodma, jnumac, jptma, jvalk, jvalr, lonuti, nbcste, nbec
    integer :: nbno, ncaba, ntyele(2), numail, nbcmp, ier, idebgd, i, nbma
    real(kind=8) :: eps, rbid
    logical :: trouv1, trouv2, trouv3, trouv4, trouv5
    character(len=3) :: k3cab, k3mai
    character(len=8) :: acier, k8b
    character(len=19) :: carte, nomrc
    character(len=24) :: cadesc, captma, cavalk, cavalr, modmai, rcvalk, rcvalr
    character(len=24) :: valk(2)
    character(len=4) :: regl
!
!
    character(len=8) :: bpela(5), etcca(4), young
    character(len=16) :: nomele(2)
!
    data          nomele /'MECA_BARRE      ','MECGSEG3        '/
    data          bpela  /'RELAX_10','MU0_RELA','F_PRG',&
     &                      'FROT_COU','FROT_LIN'/
    data          etcca  /'RELAX_10','F_PRG','COEF_FRO','PERT_LIG'/
    data          young  /'E       '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
    eps = 1.0d+02 * r8prem()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   VERIFICATION DU TYPE DES ELEMENTS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    nbno = nbnoca(icabl)
    if (quad) then
        ASSERT((mod(nbno-1,2).eq.0))
        nbma=(nbno-1)/2
    else
        nbma = nbno-1
    endif
!
!
    call jelira(numaca, 'LONUTI', lonuti)
    idecma = lonuti - nbma
    call jeveuo(numaca, 'L', jnumac)
!
    do i = 1, 2
        call jenonu(jexnom('&CATA.TE.NOMTE', nomele(i)), ntyele(i))
    end do
    modmai = modele//'.MAILLE'
    call jeveuo(modmai, 'L', jmodma)
!
    do imail = 1, nbma
        numail = zi(jnumac+idecma+imail-1)
        if ((zi(jmodma+numail-1).ne.ntyele(1)) .and. (zi(jmodma+numail-1).ne.ntyele(2))) then
            write(k3cab,'(I3)') icabl
            call utmess('F', 'MODELISA2_48', sk=k3cab)
        endif
    end do
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   RECUPERATION DES CARACTERISTIQUES DU MATERIAU ACIER
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 2.1 RECUPERATION DU MATERIAU ACIER CONSTITUANT LE CABLE
! ---
    carte = chmat//'.CHAMP_MAT '
    cavalk = carte//'.VALE'
    captma = carte//'.PTMA'
    call jeveuo(cavalk, 'L', jvalk)
    call jeveuo(captma, 'L', jptma)
!
    numail = zi(jnumac+idecma)
    ias = zi(jptma+numail-1)
    if (ias .eq. 0) then
        write(k3mai,'(I3)') numail
        write(k3cab,'(I3)') icabl
        valk(1) = k3mai
        valk(2) = k3cab
        call utmess('F', 'MODELISA2_49', nk=2, valk=valk)
    endif
!
    call dismoi('F', 'NB_CMP_MAX', 'NOMMATER', 'GRANDEUR', nbcmp,&
                k8b, ier)
!.... NBCMP COMPOSANTES POUR LA GRANDEUR NOMMATER QUI COMPOSE LA CARTE
!.... => ACCES  AU NOM DU MATERIAU ASSOCIE A UNE MAILLE
    idebgd=nbcmp*(ias-1)+1
    acier = zk8(jvalk+idebgd-1)
!.... ON VERIFIE QUE LE MEME MATERIAU A ETE AFFECTE A TOUTES LES MAILLES
!.... DU CABLE
!.... N.B. LE PASSAGE PREALABLE DANS LA ROUTINE TOPOCA GARANTIT NBNO > 2
!
    do imail = 2, nbma
        numail = zi(jnumac+idecma+imail-1)
        ias = zi(jptma+numail-1)
        if (ias .eq. 0) then
            write(k3mai,'(I3)') numail
            write(k3cab,'(I3)') icabl
            valk(1) = k3mai
            valk(2) = k3cab
            call utmess('F', 'MODELISA2_49', nk=2, valk=valk)
        endif
        idebgd=nbcmp*(ias-1)+1
        k8b = zk8(jvalk+idebgd-1)
!C         K8B = ZK8(JVALK+IAS-1)
        if (k8b .ne. acier) then
            write(k3cab,'(I3)') icabl
            call utmess('F', 'MODELISA2_50', sk=k3cab)
        endif
    end do
!
! 2.2 RELATION DE COMPORTEMENT <ELAS> DU MATERIAU ACIER
! ---
    nomrc = acier//'.ELAS      '
    rcvalk = nomrc//'.VALK'
    call jeexin(rcvalk, iret)
    if (iret .eq. 0) then
        write(k3cab,'(I3)') icabl
        call utmess('F', 'MODELISA2_51', sk=k3cab)
    endif
    rcvalr = nomrc//'.VALR'
    call jeveuo(rcvalk, 'L', jvalk)
    call jeveuo(rcvalr, 'L', jvalr)
    call jelira(rcvalr, 'LONMAX', nbcste)
!
    trouv1 = .false.
    do icste = 1, nbcste
        if (zk8(jvalk+icste-1) .eq. young) then
            trouv1 = .true.
            ea = zr(jvalr+icste-1)
            goto 31
        endif
    end do
!
 31 continue
    if (.not. trouv1) then
        write(k3cab,'(I3)') icabl
        call utmess('F', 'MODELISA2_52', sk=k3cab)
    endif
    if (ea .le. 0.0d0) then
        write(k3cab,'(I3)') icabl
        call utmess('F', 'MODELISA2_53', sk=k3cab)
    endif
!
! 2.3 RELATION DE COMPORTEMENT <BPEL_ACIER> OU <ETCC_ACIER>
!      DU MATERIAU ACIER
! ---
!
    if (regl .eq. 'BPEL') then
        nomrc = acier//'.BPEL_ACIER'
    else
        nomrc = acier//'.ETCC_ACIER'
    endif
    rcvalk = nomrc//'.VALK'
    call jeexin(rcvalk, iret)
    if (iret .eq. 0) then
        write(k3cab,'(I3)') icabl
        call utmess('F', 'MODELISA2_54', sk=k3cab)
    endif
    rcvalr = nomrc//'.VALR'
    call jeveuo(rcvalk, 'L', jvalk)
    call jeveuo(rcvalr, 'L', jvalr)
    call jelira(rcvalr, 'LONMAX', nbcste)
!
!  2.3.1 CAS BPEL
    if (regl .eq. 'BPEL') then
        trouv1 = .false.
        trouv2 = .false.
        trouv3 = .false.
        trouv4 = .false.
        trouv5 = .false.
        do icste = 1, nbcste
            if (zk8(jvalk+icste-1) .eq. bpela(1)) then
                trouv1 = .true.
                rh1000 = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. bpela(2)) then
                trouv2 = .true.
                prelax = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. bpela(3)) then
                trouv3 = .true.
                fprg = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. bpela(4)) then
                trouv4 = .true.
                frco = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. bpela(5)) then
                trouv5 = .true.
                frli = zr(jvalr+icste-1)
            endif
            if (trouv1 .and. trouv2 .and. trouv3 .and. trouv4 .and. trouv5) goto 41
        end do
!
 41     continue
    else
!       LECTURE DES DONNEES ETCC_ACIER
        trouv1 = .false.
        trouv2 = .false.
        trouv3 = .false.
        trouv4 = .false.
!
        do icste = 1, nbcste
            if (zk8(jvalk+icste-1) .eq. etcca(1)) then
                trouv1 = .true.
                rh1000 = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. etcca(2)) then
                trouv2 = .true.
                fprg = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. etcca(3)) then
                trouv3 = .true.
                frco = zr(jvalr+icste-1)
            endif
            if (zk8(jvalk+icste-1) .eq. etcca(4)) then
                trouv4 = .true.
                frli = zr(jvalr+icste-1)
            endif
            if (trouv1 .and. trouv2 .and. trouv3 .and. trouv4) goto 43
        end do
!
 43     continue
!
!     POUR ETCC, FRLI=FROTTEMENT*PERTE EN LIGNE
        frli=frli*frco
    endif
!
!
    if (rh1000 .eq. 0.0d0) then
        relax = .false.
    else
        relax = .true.
    endif
    if (relax .and. fprg .le. 0.0d0) then
        write(k3cab,'(I3)') icabl
        call utmess('F', 'MODELISA2_55', sk=k3cab)
    endif
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   RECUPERATION DE L'AIRE DE LA SECTION DROITE DU CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    carte = caelem//'.CARGENBA  '
    cadesc = carte//'.DESC'
    cavalr = carte//'.VALE'
    captma = carte//'.PTMA'
    call jeveuo(cadesc, 'L', jdesc)
    call jeveuo(cavalr, 'L', jvalr)
    call jeveuo(captma, 'L', jptma)
!
    iasmax = zi(jdesc+1)
    call jelira(jexnom('&CATA.GD.NOMCMP', 'CAGNBA'), 'LONMAX', ncaba)
!     NOMBRE D'ENTIERS CODES DANS LA CARTE
    call dismoi('F', 'NB_EC', 'CAGNBA', 'GRANDEUR', nbec,&
                k8b, iret)
!
!.... EXTRACTION DE LA VALEUR DE L'AIRE DE LA SECTION DROITE AFFECTEE
!.... A LA PREMIERE MAILLE APPARTENANT AU CABLE
    numail = zi(jnumac+idecma)
    ias = zi(jptma+numail-1)
    if (ias .eq. 0) then
        write(k3mai,'(I3)') numail
        write(k3cab,'(I3)') icabl
        valk(1) = k3mai
        valk(2) = k3cab
        call utmess('F', 'MODELISA2_57', nk=2, valk=valk)
    endif
!
    icode = zi(jdesc-1+3+2*iasmax+nbec*(ias-1)+1)
    iranv = 0
    do icmp = 1, irana1
        if (exisdg([icode],icmp)) iranv = iranv + 1
    end do
    if (iranv .eq. 0) then
        write(k3mai,'(I3)') numail
        write(k3cab,'(I3)') icabl
        valk(1) = k3mai
        valk(2) = k3cab
        call utmess('F', 'MODELISA2_58', nk=2, valk=valk)
    endif
!
    sa = zr(jvalr+ncaba*(ias-1)+iranv-1)
    if (sa .le. 0.0d0) then
        write(k3mai,'(I3)') numail
        write(k3cab,'(I3)') icabl
        valk(1) = k3mai
        valk(2) = k3cab
        call utmess('F', 'MODELISA2_59', nk=2, valk=valk)
    endif
!
!     ON VERIFIE QUE LA MEME AIRE DE SECTION DROITE A ETE AFFECTEE
!     A TOUTES LES MAILLES DU CABLE
!     LE PASSAGE PREALABLE DANS LA ROUTINE TOPOCA GARANTIT NBNO > 2
    do imail = 2, nbma
!
        numail = zi(jnumac+idecma+imail-1)
        ias = zi(jptma+numail-1)
!
        if (ias .eq. 0) then
            write(k3mai,'(I3)') numail
            write(k3cab,'(I3)') icabl
            valk(1) = k3mai
            valk(2) = k3cab
            call utmess('F', 'MODELISA2_57', nk=2, valk=valk)
        endif
!
        icode = zi(jdesc-1+3+2*iasmax+nbec*(ias-1)+1)
        iranv = 0
        do icmp = 1, irana1
            if (exisdg([icode],icmp)) iranv = iranv + 1
        end do
        if (iranv .eq. 0) then
            write(k3mai,'(I3)') numail
            write(k3cab,'(I3)') icabl
            valk(1) = k3mai
            valk(2) = k3cab
            call utmess('F', 'MODELISA2_58', nk=2, valk=valk)
        endif
        rbid = zr(jvalr+ncaba*(ias-1)+iranv-1)
        if (dble(abs(rbid-sa))/sa .gt. eps) then
            write(k3cab,'(I3)') icabl
            call utmess('F', 'MODELISA2_60', sk=k3cab)
        endif
!
    end do
!
    call jedema()
!
end subroutine
