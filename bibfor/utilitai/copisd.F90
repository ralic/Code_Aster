subroutine copisd(typesd, base, sd1, sd2)
! aslint: disable=W1501
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copich.h"
#include "asterfort/copis2.h"
#include "asterfort/cpclma.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedup1.h"
#include "asterfort/jedupc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rscopi.h"
#include "asterfort/tbcopi.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: typesd, base, sd1, sd2
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!
!   BUT:
!   DUPLIQUER UNE STRUCTURE DE DONNEES SOUS UN AUTRE NOM.
!   (SI SD2 EXISTE DEJA, ON LA DETRUIT AVANT DE LA RECREER)
!
!     IN:
!     TYPESD  : TYPE DE LA SD A DUPPLIQUER
!               ' ' (INCONNU)
!               'CHAMP' (OU 'CHAMP_GD')
!               'FONCTION'  (POUR FONCTIONS ET NAPPES)
!               'CORRESP_2_MAILLA'
!               'LISTR8'          'LISTIS'
!               'CHAM_NO_S'       'CHAM_ELEM_S'
!               'VARI_COM'        'TABLE'
!               'RESULTAT'        'NUME_DDL'
!               'MAILLAGE'        'LIGREL'
!               'MATR_ASSE_GENE'  'MATR_ASSE'
!               'PROF_CHNO'       'MATR_ELEM'
!               'VECT_ELEM'       'SOLVEUR'
!     BASE     : 'G' , 'V' , ... : BASE DE CREATION DE SD2
!     SD1 (K*) : NOM DE LA SD A DUPPLIQUER
!     SD2 (K*) : NOM DE LA SD A CREER
!
!     OUT:
!     SD2 EST CREEE ET A LE MEME CONTENU QUE SD1
!
!-----------------------------------------------------------------------
!
    integer :: iret, i, nbtu, jltn1, jltn2, idd, nbsd, ilimpi
    character(len=1) :: bas2
    character(len=8) :: k8b, k81, k82
    character(len=12) :: k121, k122
    character(len=14) :: com1, com2, nu1, nu2
    character(len=16) :: typ2sd, corr1, corr2
    character(len=19) :: ch1, ch2, sdr1, k191, k192
    character(len=24) :: masfe1, masfe2, x1, x2
    integer :: ifetm1, ifetm2, j1, iexi
    logical :: lfeti
!
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
    bas2 = base
    x1=sd1
    x2=sd2
    call assert(x1.ne.x2)
!
! ----------------------------------------------------------------------
!     SUPRESSION DE SD2 :
    call detrsd(typesd, sd2)
!
! ----------------------------------------------------------------------
    if (typesd .eq. ' ') then
!     -----------------------
!       -- TYPESD INCONNU => ON UTILISE JEDUPC => COUTEUX EN CPU
        call jedupc(' ', sd1, 1, base, sd2,&
                    .true.)
!
! ----------------------------------------------------------------------
    else if ((typesd.eq.'CHAMP') .or. (typesd.eq.'CHAMP_GD')) then
!     ----------------------------------------------------------------
        ch1 = sd1
        ch2 = sd2
        call copich(bas2, ch1, ch2)
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'VARI_COM') then
!     -----------------------------------
        com1 = sd1
        com2 = sd2
!
        call exisd('CHAMP', com1//'.TEMP', iret)
        if (iret .gt. 0) call copich(bas2, com1//'.TEMP', com2//'.TEMP')
        call exisd('CHAMP', com1//'.HYDR', iret)
        if (iret .gt. 0) call copich(bas2, com1//'.HYDR', com2//'.HYDR')
        call exisd('CHAMP', com1//'.SECH', iret)
        if (iret .gt. 0) call copich(bas2, com1//'.SECH', com2//'.SECH')
        call exisd('CHAMP', com1//'.PHAS', iret)
        if (iret .gt. 0) call copich(bas2, com1//'.PHAS', com2//'.PHAS')
        call exisd('CHAMP', com1//'.EPAN', iret)
        if (iret .gt. 0) call copich(bas2, com1//'.EPAN', com2//'.EPAN')
        call exisd('CHAMP', com1//'.INST', iret)
        if (iret .gt. 0) call copich(bas2, com1//'.INST', com2//'.INST')
        call exisd('CHAMP', com1//'.TOUT', iret)
        if (iret .gt. 0) call copich(bas2, com1//'.TOUT', com2//'.TOUT')
!
        call jedup1(com1//'.EXISTENCE', bas2, com2//'.EXISTENCE')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'SOLVEUR') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
!
        call jedup1(k191//'.SLVK', bas2, k192//'.SLVK')
        call jedup1(k191//'.SLVI', bas2, k192//'.SLVI')
        call jedup1(k191//'.SLVR', bas2, k192//'.SLVR')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'LISTE_RELA') then
!     -------------------------------------
        k191 = sd1
        k192 = sd2
!
        call jedup1(k191//'.RLLA', bas2, k192//'.RLLA')
        call jedup1(k191//'.RLBE', bas2, k192//'.RLBE')
        call jedup1(k191//'.RLSU', bas2, k192//'.RLSU')
        call jedup1(k191//'.RLTC', bas2, k192//'.RLTC')
        call jedup1(k191//'.RLNO', bas2, k192//'.RLNO')
        call jedup1(k191//'.RLCO', bas2, k192//'.RLCO')
        call jedup1(k191//'.RLNT', bas2, k192//'.RLNT')
        call jedup1(k191//'.RLPO', bas2, k192//'.RLPO')
        call jedup1(k191//'.RLNR', bas2, k192//'.RLNR')
        call jedup1(k191//'.RLTV', bas2, k192//'.RLTV')
        call jedup1(k191//'.RLDD', bas2, k192//'.RLDD')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'FONCTION') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
!
        call jedup1(k191//'.PARA', bas2, k192//'.PARA')
        call jedup1(k191//'.PROL', bas2, k192//'.PROL')
        call jedup1(k191//'.VALE', bas2, k192//'.VALE')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'LISTR8'.or.typesd.eq.'LISTIS') then
!     --------------------------------------------------
        k191 = sd1
        k192 = sd2
!
        call jedup1(k191//'.LPAS', bas2, k192//'.LPAS')
        call jedup1(k191//'.NBPA', bas2, k192//'.NBPA')
        call jedup1(k191//'.BINT', bas2, k192//'.BINT')
        call jedup1(k191//'.VALE', bas2, k192//'.VALE')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'CORRESP_2_MAILLA') then
!        ---------------------------------
        corr1 = sd1
        corr2 = sd2
        call jedup1(corr1//'.PJXX_K1', bas2, corr2//'.PJXX_K1')
        call jedup1(corr1//'.PJEF_NU', bas2, corr2//'.PJEF_NU')
        call jedup1(corr1//'.PJEF_NB', bas2, corr2//'.PJEF_NB')
        call jedup1(corr1//'.PJEF_CF', bas2, corr2//'.PJEF_CF')
        call jedup1(corr1//'.PJEF_M1', bas2, corr2//'.PJEF_M1')
        call jedup1(corr1//'.PJEF_TR', bas2, corr2//'.PJEF_TR')
        call jedup1(corr1//'.PJEF_CO', bas2, corr2//'.PJEF_CO')
        call jedup1(corr1//'.PJEF_MP', bas2, corr2//'.PJEF_MP')
        call jedup1(corr1//'.PJEF_EL', bas2, corr2//'.PJEF_EL')
        call jedup1(corr1//'.PJNG_I1', bas2, corr2//'.PJNG_I1')
        call jedup1(corr1//'.PJNG_I2', bas2, corr2//'.PJNG_I2')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'CHAM_NO_S') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.CNSD', bas2, k192//'.CNSD')
        call jedup1(k191//'.CNSK', bas2, k192//'.CNSK')
        call jedup1(k191//'.CNSC', bas2, k192//'.CNSC')
        call jedup1(k191//'.CNSL', bas2, k192//'.CNSL')
        call jedup1(k191//'.CNSV', bas2, k192//'.CNSV')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'CHAM_ELEM_S') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.CESD', bas2, k192//'.CESD')
        call jedup1(k191//'.CESK', bas2, k192//'.CESK')
        call jedup1(k191//'.CESC', bas2, k192//'.CESC')
        call jedup1(k191//'.CESL', bas2, k192//'.CESL')
        call jedup1(k191//'.CESV', bas2, k192//'.CESV')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'PROF_CHNO') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.DEEQ', bas2, k192//'.DEEQ')
        call jedup1(k191//'.NUEQ', bas2, k192//'.NUEQ')
        call jedup1(k191//'.PRNO', bas2, k192//'.PRNO')
        call jedup1(k191//'.LILI', bas2, k192//'.LILI')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'NUME_EQUA') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
        call copis2('PROF_CHNO', bas2, k191, k192)
        call jedup1(k191//'.NEQU', bas2, k192//'.NEQU')
        call jedup1(k191//'.REFN', bas2, k192//'.REFN')
        call jedup1(k191//'.DELG', bas2, k192//'.DELG')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'STOCKAGE') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.SCBL', bas2, k192//'.SCBL')
        call jedup1(k191//'.SCDI', bas2, k192//'.SCDI')
        call jedup1(k191//'.SCDE', bas2, k192//'.SCDE')
        call jedup1(k191//'.SCHC', bas2, k192//'.SCHC')
        call jedup1(k191//'.SCIB', bas2, k192//'.SCIB')
!
        call jedup1(k191//'.SMDI', bas2, k192//'.SMDI')
        call jedup1(k191//'.SMDE', bas2, k192//'.SMDE')
        call jedup1(k191//'.SMHC', bas2, k192//'.SMHC')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'NUME_DDL') then
!     -----------------------------------
        nu1 = sd1
        nu2 = sd2
        call copis2('NUME_EQUA', bas2, nu1//'.NUME', nu2//'.NUME')
        call copis2('STOCKAGE', bas2, nu1//'.SLCS', nu2//'.SLCS')
        call copis2('STOCKAGE', bas2, nu1//'.SMOS', nu2//'.SMOS')
        call jedup1(nu1//'.NSLV', bas2, nu2//'.NSLV')
!
! --------------------------------------------------------------------
        else if (typesd.eq.'MATR_ASSE_GENE' .or. typesd.eq.'MATR_ASSE')&
    then
!     ---------------------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.CCID', bas2, k192//'.CCID')
        call jedup1(k191//'.CCII', bas2, k192//'.CCII')
        call jedup1(k191//'.CCLL', bas2, k192//'.CCLL')
        call jedup1(k191//'.CCVA', bas2, k192//'.CCVA')
        call jedup1(k191//'.CONL', bas2, k192//'.CONL')
        call jedup1(k191//'.DESC', bas2, k192//'.DESC')
        call jedup1(k191//'.DIGS', bas2, k192//'.DIGS')
        call jedup1(k191//'.LIME', bas2, k192//'.LIME')
        call jedup1(k191//'.REFA', bas2, k192//'.REFA')
        call jedup1(k191//'.UALF', bas2, k192//'.UALF')
        call jedup1(k191//'.VALF', bas2, k192//'.VALF')
        call jedup1(k191//'.WALF', bas2, k192//'.WALF')
        call jedup1(k191//'.VALM', bas2, k192//'.VALM')
!
! FETI OR NOT ?
        masfe1 = k191//'.FETM'
        call jeexin(masfe1, iret)
        if (iret .gt. 0) then
            lfeti = .true.
        else
            lfeti = .false.
        endif
!
        if (lfeti) then
            masfe2 = k192//'.FETM'
            call jedup1(masfe1, bas2, masfe2)
            call jedup1(k191//'.FETF', bas2, k192//'.FETF')
            call jedup1(k191//'.FETP', bas2, k192//'.FETP')
            call jedup1(k191//'.FETR', bas2, k192//'.FETR')
!
            call jelira(masfe1, 'LONMAX', nbsd, k8b)
            call jeveuo(masfe1, 'L', ifetm1)
            call jeveuo(masfe2, 'E', ifetm2)
            call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
            do 30 idd = 1, nbsd
                if (zi(ilimpi+idd) .eq. 1) then
                    zk24(ifetm2+idd-1) = zk24(ifetm1+idd-1)
                endif
30          continue
!
        endif
!
!
! --------------------------------------------------------------------
    else if (typesd.eq.'TABLE') then
!     -----------------------------------
        call exisd(typesd, sd1, iret)
        if (iret .eq. 0) then
            sdr1 = sd1
            call u2mesk('F', 'UTILITAI_40', 1, sdr1)
        endif
!
        call tbcopi(base, sd1, sd2)
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'RESULTAT') then
!     -----------------------------------
        call exisd(typesd, sd1, iret)
        if (iret .eq. 0) then
            sdr1 = sd1
            call u2mesk('F', 'UTILITAI_40', 1, sdr1)
        endif
!
        call rscopi(base, sd1, sd2)
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'VOISINAGE') then
!     -----------------------------------
        k121 = sd1
        k122 = sd2
        call jedup1(k121//'.PTVOIS', bas2, k122//'.PTVOIS')
        call jedup1(k121//'.ELVOIS', bas2, k122//'.ELVOIS')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'LIGREL') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.LGNS', bas2, k192//'.LGNS')
        call jedup1(k191//'.LIEL', bas2, k192//'.LIEL')
        call jedup1(k191//'.NEMA', bas2, k192//'.NEMA')
        call jedup1(k191//'.LGRF', bas2, k192//'.LGRF')
        call jedup1(k191//'.NBNO', bas2, k192//'.NBNO')
        call jedup1(k191//'.NVGE', bas2, k192//'.NVGE')
        call jedup1(k191//'.PRNM', bas2, k192//'.PRNM')
        call jedup1(k191//'.PRNS', bas2, k192//'.PRNS')
        call jedup1(k191//'.REPE', bas2, k192//'.REPE')
        call jedup1(k191//'.SSSA', bas2, k192//'.SSSA')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'CABL_PRECONT') then
!     -----------------------------------
        k81 = sd1
        k82 = sd2
        call copich(bas2, k81//'.CHME.SIGIN', k82//'.CHME.SIGIN')
        call copis2('LISTE_RELA', bas2, k81//'.LIRELA', k82//'.LIRELA')
        call copis2('L_TABLE', bas2, k81, k82)
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'SQUELETTE') then
!     -----------------------------------
        k81 = sd1
        k82 = sd2
        call copis2('MAILLAGE', bas2, k81, k82)
!
        call jedup1(k81//'.CORRES', bas2, k82//'.CORRES')
        call jedup1(k81//'.INV.SKELETON', bas2, k82//'.INV.SKELETON')
        call jedup1(k81//'         .NOMSST', bas2, k82//'         .NOMSST')
        call jedup1(k81//'.ANGL_NAUT', bas2, k82//'.ANGL_NAUT')
        call jedup1(k81//'.TRANS', bas2, k82//'.TRANS')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'L_TABLE') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
!
        call jeexin(k191//'.LTNT', iret)
        if (iret .ne. 0) then
            call jedup1(k191//'.LTNS', bas2, k192//'.LTNS')
            call jedup1(k191//'.LTNT', bas2, k192//'.LTNT')
            call jelira(k191//'.LTNT', 'LONUTI', nbtu, k8b)
            call jeveuo(k191//'.LTNS', 'L', jltn1)
            call jeveuo(k192//'.LTNS', 'E', jltn2)
            k192(1:8) = k192
            do 10 i = 1, nbtu
                k191 = zk24(jltn1+i-1)(1:19)
                k192(9:19) = k191(9:19)
                call exisd('TABLE', k191, iret)
                if (iret .ne. 0) then
                    call tbcopi(bas2, k191, k192)
                else
                    call u2mesk('F', 'UTILITAI_41', 1, k191)
                endif
                zk24(jltn2+i-1) = k192
10          continue
        endif
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'MAILLAGE') then
!     -----------------------------------
        k81 = sd1
        k82 = sd2
        call copich(bas2, k81//'.COORDO', k82//'.COORDO')
        call copich(bas2, k81//'.ABS_CURV', k82//'.ABS_CURV')
!       -- LES 2 CHAMPS COPIES DOIVENT S'APPUYER
!          SUR LE NOUVEAU MAILLAGE :
        call jeveuo(k82//'.COORDO    .REFE', 'E', j1)
        zk24(j1-1+1)=k82
        call jeexin(k82//'.ABS_CURV  .NOMA', iexi)
        if (iexi .gt. 0) then
            call jeveuo(k82//'.ABS_CURV  .NOMA', 'E', j1)
            zk8(j1-1+1)=k82
        endif
!
!
        call jedup1(k81//'.ADAPTATION', bas2, k82//'.ADAPTATION')
        call jedup1(k81//'.DIME', bas2, k82//'.DIME')
        call jedup1(k81//'.NOMNOE', bas2, k82//'.NOMNOE')
        call cpclma(k81, k82, 'GROUPENO', bas2)
        call jedup1(k81//'.NOMMAI', bas2, k82//'.NOMMAI')
        call jedup1(k81//'.TYPMAIL', bas2, k82//'.TYPMAIL')
        call jedup1(k81//'.CONNEX', bas2, k82//'.CONNEX')
        call cpclma(k81, k82, 'GROUPEMA', bas2)
        call jedup1(k81//'.NOMACR', bas2, k82//'.NOMACR')
        call jedup1(k81//'.PARA_R', bas2, k82//'.PARA_R')
        call jedup1(k81//'.SUPMAIL', bas2, k82//'.SUPMAIL')
        call jedup1(k81//'.TYPL', bas2, k82//'.TYPL')
        call jedup1(k81//'.TITR', bas2, k82//'.TITR')
!
        call copis2('L_TABLE', bas2, k81, k82)
        call jedup1(k81//'           .TITR', bas2, k82//'           .TITR')
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'MODELE') then
!     -----------------------------------
        k81 = sd1
        k82 = sd2
        call copis2('LIGREL', bas2, k81//'.MODELE', k82//'.MODELE')
        call copis2('L_TABLE', bas2, k81, k82)
!
        call jedup1(k81//'.NOEUD', bas2, k82//'.NOEUD')
        call jedup1(k81//'.MAILLE', bas2, k82//'.MAILLE')
        call jedup1(k81//'.PARTIT', bas2, k82//'.PARTIT')
!
!       -- IL FAUT METTRE A JOUR LGRF(2):
        call jeexin(k82//'.MODELE    .LGRF', iexi)
        if (iexi .gt. 0) then
            call jeveuo(k82//'.MODELE    .LGRF', 'E', j1)
            zk8(j1-1+2)=k82
        endif
!
!
! ----------------------------------------------------------------------
    else if (typesd.eq.'MATR_ELEM' .or. typesd.eq.'VECT_ELEM') then
!     ---------------------------------------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.RELR', bas2, k192//'.RELR')
        call jedup1(k191//'.RERR', bas2, k192//'.RERR')
        call jedup1(k191//'.RELC', bas2, k192//'.RELC')
        call jedup1(k191//'.RECC', bas2, k192//'.RECC')
        call jedup1(k191//'.TITR', bas2, k192//'.TITR')
!       JE (JP) NE SAIS PAS FAIRE UNE COPIE "PROFONDE" :
!       QUEL NOM DONNER AUX CH2 ?
!       A PART _00000I, JE NE VOIS PAS ...
!
!       CALL JEEXIN(K192//'.RELR',IEXI)
!       IF (IEXI.GT.0) THEN
!         CALL JEVEUO(K191//'.RELR','E',JRELR1)
!         CALL JEVEUO(K192//'.RELR','E',JRELR2)
!         CALL JELIRA(K191//'.RELR','LONUTI',N1,KBID)
!         CALL JELIRA(K192//'.RELR','LONUTI',N2,KBID)
!         CALL ASSERT(N1.EQ.N2)
!         DO 20,K=1,N1
!           CH1=ZK24(JRELR1-1+K)
!           CH2=??
!           CALL COPICH(BAS2,CH1,CH2)
!           ZK24(JRELR2-1+K)=CH2
! 20      CONTINUE
!       ENDIF
!     ------------------------------------------------------------------
    else if (typesd.eq.'LISTE_CHARGES') then
!     -----------------------------------
        k191 = sd1
        k192 = sd2
        call jedup1(k191//'.INFC', bas2, k192//'.INFC')
        call jedup1(k191//'.LCHA', bas2, k192//'.LCHA')
        call jedup1(k191//'.FCHA', bas2, k192//'.FCHA')
!
!
! ----------------------------------------------------------------------
    else
        typ2sd = typesd
        call u2mesk('F', 'UTILITAI_42', 1, typ2sd)
    endif
!
    call jedema()
end subroutine
