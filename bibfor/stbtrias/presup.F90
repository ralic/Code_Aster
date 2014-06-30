subroutine presup(iunv, imod, lgrcou)
    implicit none
#include "asterfort/ecrelt.h"
#include "asterfort/ecrneu.h"
#include "asterfort/inistb.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jjmmaa.h"
#include "asterfort/slecol.h"
#include "asterfort/slecor.h"
#include "asterfort/sleelt.h"
#include "asterfort/slegeo.h"
#include "asterfort/slegro.h"
#include "asterfort/sleneu.h"
#include "asterfort/snecol.h"
#include "asterfort/utmess.h"
    logical(kind=1) :: lgrcou
! ======================================================================
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
!A PRESUPER
!  =============================================================
!  !                                                           !
!  !  FONCTION : INTERFACE ENTRE SUPERTAB I-DEAS(4.0) - ASTER  !
!  !                             SUPERTAB I-DEAS(6.0) - ASTER  !
!  !                             SUPERTAB I-DEAS(7.0) - ASTER  !
!  !                                                           !
!  !  DANS CETTE INTERFACE NE SONT RETENUS QUE LES DATASETS    !
!  !  SUIVANTS :                                               !
!  !                                                           !
!  !  DATASET N  151 (SUPERTAB 4,6,7)  ---> TITRE              !
!  !  DATASET N  18  (SUPERTAB 4)      ---> SYS DE COORD       !
!  !          N  2420 (SUPERTAB 5-10)  --->                    !
!  !  DATASET N   15 (SUPERTAB 4)      ---> COORDONNEES DES    !
!  !          N  781 (SUPERTAB 6)      --->   NOEUDS           !
!  !          N 2411 (SUPERTAB 7-10)   --->                    !
!  !  DATASET N   71 (SUPERTAB 4)      ---> DESCRIPTION DES    !
!  !          N  780 (SUPERTAB 6)      --->   ELEMENTS         !
!  !          N 2412 (SUPERTAB 7-10)   --->                    !
!  !  DATASET N  752 (SUPERTAB 4 & 6)  ---> GROUPES DE NOEUDS  !
!  !          N 2417 (SUPERTAB 7)      --->   OU MAILLES       !
!  !          N 2428 (MASTER SERIES 3) --->                    !
!  !          N 2429 (MASTER SERIES 3) --->                    !
!  !          N 2430 (MASTER SERIES 3) --->                    !
!  !          N 2432 (MASTER SERIES 3) --->                    !
!  !          N 2435 (SUPERTAB 7)      --->                    !
!  !          N 2452 (MASTER SERIES 3) --->                    !
!  !          N 2467 (MASTER SERIES 3) --->                    !
!  !          N 2477 (MASTER SERIES v11) --->                  !
!  !  DATASET N  735 (SUPERTAB 4 & 6)  ---> DESCRIPTION        !
!  !              ??                        GEOMETRIQUE        !
!  =============================================================
!  !                                                           !
!  !  ROUTINES APPELEES :                                      !
!  !                          : IUNIFI (FONCTION)              !
!  !                          : INISTB                         !
!  !                          : SLETIT                         !
!  !                          : SLENEU                         !
!  !                          : ECRNEU                         !
!  !                          : SLEELT                         !
!  !                          : ECRELT                         !
!  !                          : SLEGRO                         !
!  !                          : SLEGEO                         !
!  !                                                           !
!  =============================================================
!
! --> DECLARATIONS DES VARIABLES
!
    logical(kind=1) :: larret
    integer :: min, man, imod, imes, iunv, mxtyma, mxperm, mxperf
    integer :: maxfa, i, nbtyma, ites, maxnod
    parameter (mxtyma=99,maxnod=32,mxperm=maxnod*mxtyma)
    parameter (maxfa=6,mxperf=maxfa*mxtyma)
    integer :: limail(mxtyma), nbmtot
    integer :: datset, nbnode, nbmail(mxtyma), indic(mxtyma), permut(mxperm)
    integer :: indicf(mxtyma), permuf(mxperf)
    integer :: mint(mxtyma), mant(mxtyma), io
    character(len=4) :: ct(3)
    character(len=6) :: char, moins1
    character(len=8) :: nomail(mxtyma), rquoi
    character(len=12) :: aut
    real(kind=8) :: ama, ami, bma, bmi, cma, cmi
!
!
! --> INITIALISATIONS
!
    moins1 = '    -1'
    rquoi = '????????'
    larret=.true.
!
!  -->N  D'UNITE LOGIQUE DES FICHIERS
!
    imes = iunifi('MESSAGE')
!
    do 1234 i = 1, mxtyma
        nbmail(i) = 0
        nomail(i) = rquoi
1234  end do
!
    call inistb(maxnod, nbtyma, nomail, indic, permut,&
                limail, indicf, permuf, maxfa)
!
!     RECHERCHE DU PREMIER '    -1'
!
1000  continue
    read (iunv,'(A)',end=9999) char
    if (char .ne. moins1) goto 1000
!
!   QUOIQU'IL ARRIVE, ON ECRIT DANS LE TITRE QUE LE MAILLAGE
!   A ETE LU AU FORMAT IDEAS :
!   ---------------------------------------------------------------
    write (imod,'(A,4X,A)')'TITRE','NOM=INDEFINI'
    call jjmmaa(ct, aut)
    write(imod,'(9X,A,17X,A,A2,A,A2,A,A4)')'AUTEUR=INTERFACE_IDEAS'&
     &    ,'DATE=', ct(1)(1:2),'/',ct(2)(1:2),'/',ct(3)
    write (imod,'(A)') 'FINSF'
    write (imod,'(A)') '%'
!
!
 1  continue
!
    read (iunv,'(I6)',end=9999, iostat=io) datset
!     LORSQU'ON A UN ECHEC LORS DE LA LECTURE DU DATASET
!     ON INITIALISE DATASET A 0
    if (io .gt. 0) datset=0
!
    if (datset .eq. -1) then
!  -->   FIN DE DATASET
!
    else if ((datset.eq.18).or.(datset.eq.2420)) then
!
!  -->   LECTURE ET ECRITURE DU(DES) SYSTEME(S) DE COORDONNEES
!
        call slecor(iunv, datset)
!
        else if ((datset.eq.15).or.(datset.eq.781) .or.(datset.eq.2411))&
    then
!
!  -->   LECTURE ET ECRITURE DES  NOEUDS
        call sleneu(iunv, nbnode, ama, bma, cma,&
                    ami, bmi, cmi, min, man,&
                    ites, datset)
        larret=.false.
        call ecrneu(imod, nbnode, ama, bma, cma,&
                    ami, bmi, cmi, min, man,&
                    ites)
        if (lgrcou) call snecol(imod, nbnode)
!
!
        else if ((datset.eq.71).or.(datset.eq.780) .or.(datset.eq.2412)&
    .or. (datset.eq.2431).or.(datset.eq.82)) then
!
!  -->   LECTURE ET ECRITURE DES  MAILLES
        call sleelt(iunv, maxnod, nbtyma, indic, permut,&
                    nbmail, mint, mant, datset, nbmtot)
        larret=.false.
        call ecrelt(imod, maxnod, nbtyma, nomail, nbmail,&
                    mint, mant, limail, nbmtot)
        if (lgrcou) call slecol(imod, nbmtot)
!
        else if (datset.eq.752.or.datset.eq.2417 .or.datset.eq.2428.or.&
    datset.eq.2429 .or.datset.eq.2430.or.datset.eq.2432 .or.datset.eq.&
    2435.or.datset.eq.2452 .or.datset.eq.2467.or.datset.eq.2477) then
!
!  -->   LECTURE ET ECRITURE DES GROUPES DE NOEUDS OU D'MAILLES
        call slegro(iunv, imod, datset)
        larret=.false.
!
    else if (datset.eq.735) then
!
!  -->   LECTURE ET ECRITURE DES NOEUDS ET MAILLES RATTACHES AUX
!        CURVES,MESHS AREA ET MESHS VOLUME
        call slegeo(iunv, imod)
!
    else
!
!  -->   LECTURE D'UNE RUBRIQUE INCONNUE
        write (imes,*) 'ON NE TRAITE PAS LE DATASET:',datset
 2      continue
        read (iunv,'(A)',end=9999) char
        if (char .ne. moins1) goto 2
    endif
    goto 1
9999  continue
    if (larret) then
        call utmess('F', 'STBTRIAS_1')
    endif
!
    call jedetr('&&PRESUP.INFO.NOEUDS')
    call jedetr('&&PRESUP.COOR.NOEUDS')
    call jedetr('&&PRESUP.INFO.MAILLE')
    call jedetr('&&PRESUP.CONN.MAILLE')
    call jedetr('&&IDEAS.SYST')
end subroutine
