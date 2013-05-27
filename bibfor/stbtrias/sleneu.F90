subroutine sleneu(iunv, nbnode, ama, bma, cma,&
                  ami, bmi, cmi, mix, man,&
                  ites, datset)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRS_512
    implicit none
!     ==============================================================
!A PRESUPER
!
!     ==============================================================
!     !                                                            !
!     !  FONCTION:LECTURE SUR LE FICHIER UNIVERSEL ISSU DE SUPER-  !
!     !           TAB I-DEAS 4.0, 6.0 OU 7.0   DES COORDONNEES DES !
!     !           DES NOEUDS ET STOCKAGE DANS OBJETS JEVEUX        !
!     !                                                            !
!     ==============================================================
!     !                                                            !
!     !  ROUTINES APPELES : CODENT                                 !
!     !                          : IUNIFI (FONCTION)               !
!     !                                                            !
!     !  ROUTINE APPELANTE : PRESUP                                !
!     !                                                            !
!     ==============================================================
!     !                                                            !
!     !                  **************                            !
!     !                  *  ARGUMENT  *                            !
!     !                  **************                            !
!     !                                                            !
!     !  ********************************************************  !
!     !  *   NOM    *  TYPE * MODE *ALTERE *      ROLE          *  !
!     !  ********************************************************  !
!     !  *          *       *      *       *                    *  !
!     !  * NBNODE   *INTEGER*SORTIE* NON   *NBRE TOTAL DE NOEUDS*  !
!     !  *          *       *      *       *                    *  !
!     !  * AMA      *D.PRECI*SORTIE* NON   * X(MAXIMUM)         *  !
!     !  *          *       *      *       *                    *  !
!     !  * BMA      *D.PRECI*SORTIE* NON   * Y(MAXIMUM)         *  !
!     !  *          *       *      *       *                    *  !
!     !  * CMA      *D.PRECI*SORTIE* NON   * Z(MAXIMUM)         *  !
!     !  *          *       *      *       *                    *  !
!     !  * AMI      *D.PRECI*SORTIE* NON   * X(MINIMUM)         *  !
!     !  *          *       *      *       *                    *  !
!     !  * BMI      *D.PRECI*SORTIE* NON   * Y(MINIMUM)         *  !
!     !  *          *       *      *       *                    *  !
!     !  * CMI      *D.PRECI*SORTIE* NON   * Z(MINIMUM)         *  !
!     !  *          *       *      *       *                    *  !
!     !  * MAN      *INTEGER*SORTIE* NON   * N DE NOEUD MAXI    *  !
!     !  *          *       *      *       *                    *  !
!     !  * MIX      *INTEGER*SORTIE* NON   * N DE NOEUD MINI    *  !
!     !  *          *       *      *       *                    *  !
!     !  * ITES     *INTEGER*SORTIE* NON   * INDIQUE S'IL EXISTE*  !
!     !  *          *       *      *       * AU MOINS DE SYST.  *  !
!     !  *          *       *      *       * DE COORDONNEES     *  !
!     !  *          *       *      *       *                    *  !
!     !  * DATSET   *INTEGER*ENTREE* NON   * NUMERO DU DATASET  *  !
!     !  *          *       *      *       * TRAITE             *  !
!     !  *          *       *      *       *                    *  !
!     !  ********************************************************  !
!     !                                                            !
!     ==============================================================
!  --> DECLARATION DES ARGUMENTS
    include 'jeveux.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbnode, mix, man, ites, datset
    real(kind=8) :: ama, bma, cma, ami, bmi, cmi
!  --> DECLARATION DES VARIABLES LOCALES
    character(len=80) :: cbuf
    integer :: node, i, icnode, ind, j, itmp, inus, jsys
    real(kind=8) :: x, y, z
!
!  ---------- FIN DECLARATION -----------
!
!-----------------------------------------------------------------------
    integer :: imes, ire1, ire2, iret, isyst, iter
    integer :: iunv, jcoor, jinfo, ndeca, niter, ntail
!-----------------------------------------------------------------------
    call jemarq()
    nbnode=0
    ites=0
!
!  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
    imes=iunifi('MESSAGE')
!
    ntail = 1000
    niter = 1000
    ndeca = 0
    itmp = -6
    inus = 10
!
    call jeexin('&&PRESUP.INFO.NOEUDS', ire1)
    if (ire1 .ne. 0) call jedetr('&&PRESUP.INFO.NOEUDS')
    call wkvect('&&PRESUP.INFO.NOEUDS', 'V V I', 3*ntail, jinfo)
    call jeexin('&&PRESUP.COOR.NOEUDS', ire2)
    if (ire2 .ne. 0) call jedetr('&&PRESUP.COOR.NOEUDS')
    call wkvect('&&PRESUP.COOR.NOEUDS', 'V V R', 3*ntail, jcoor)
!
! -->  GESTION DES SYSTEMES DE COORDONNEES - PREMIERE PARTIE
! -->  ON TESTE L'EXISTENCE D'UN SYSTEME DE COORDONNEES (DATASET 2420)
!
    call jeexin('&&IDEAS.SYST', iret)
    if (iret .eq. 0) then
        call u2mess('I', 'STBTRIAS_9')
!     Il n'y a pas de sys de coord defini dans le fichier, pour ne pas
!     planter on en cree un bidon ici qu'on declare comme cartesien
        isyst = 0
    endif
!
 1  continue
    do 10 iter = 1, niter
        read (iunv,'(A)') cbuf
        read (unit=cbuf,fmt='(4X,I2)') ind
        if (ind .eq. -1) goto 99
!
! --> LES COORDONNEES DES NOEUDS SONT EN SIMPLE PRECISION (SUPERTAB 4
!     OU 6) OU EN REAL*8 (SUPERTAB 6 OU 7)
!
        if (datset .eq. 15) then
            read (cbuf,'(4I10,3E13.6)') node,i,j,icnode,x,y,z
        else if (datset.eq.781.or.datset.eq.2411) then
            read (cbuf,'(4I10)') node,i,j,icnode
            read (iunv,'(3E25.16)') x,y,z
        endif
!
!
! -->  GESTION DES SYSTEMES DE COORDONNEES - BIS
! -->  SI UN SYSTEME EST DEFINI, ON LE RECUPERE
!
        call jeexin('&&IDEAS.SYST', iret)
        if (iret .ne. 0) then
            call jeveuo('&&IDEAS.SYST', 'L', jsys)
            isyst = zi(jsys-1+i)
        endif
!
!        On ne teste ici que si le systeme de coordonnne est cartesien,
!        cylindrique ou autre
        if (isyst .ne. 0) then
            call u2mess('F', 'STBTRIAS_10')
        endif
!        On ne teste ici que si les noeuds font reference a plusieurs
!        systeme de coordonnne
!        On ne teste pas si ces systemes sont identiques juste si leur
!        label est different
        if ((i.ne.itmp) .and. (itmp.ne.-6)) then
            call u2mess('A', 'STBTRIAS_11')
        endif
!
!
!  --> INITIALISATION POUR LA RECHERCHE DES MINI ET MAXI
        if (nbnode .eq. 0) then
            ama=x
            bma=y
            cma=z
            ami=x
            bmi=y
            cmi=z
        else
            ama=max(ama,x)
            bma=max(bma,y)
            cma=max(cma,z)
            ami=min(ami,x)
            bmi=min(bmi,y)
            cmi=min(cmi,z)
        endif
!
        if (nbnode .eq. 0) then
            mix=node
        else
            man=max(mix,node)
        endif
!
        nbnode = nbnode + 1
!
        zi(jinfo-1+ndeca+(iter-1)*3+1) = node
        zi(jinfo-1+ndeca+(iter-1)*3+2) = i
        zi(jinfo-1+ndeca+(iter-1)*3+3) = icnode
        zr(jcoor-1+ndeca+(iter-1)*3+1) = x
        zr(jcoor-1+ndeca+(iter-1)*3+2) = y
        zr(jcoor-1+ndeca+(iter-1)*3+3) = z
10  end do
    ntail = ntail + niter
    ndeca = ndeca + 3000
    call juveca('&&PRESUP.INFO.NOEUDS', 3*ntail)
    call jeveuo('&&PRESUP.INFO.NOEUDS', 'E', jinfo)
    call juveca('&&PRESUP.COOR.NOEUDS', 3*ntail)
    call jeveuo('&&PRESUP.COOR.NOEUDS', 'E', jcoor)
    goto 1
99  continue
!
    write (imes,*) 'NOMBRE DE NOEUDS :',nbnode
    call jedema()
end subroutine
