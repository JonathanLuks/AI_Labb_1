   M S B A M L   `   `   `  ����B  >Reversi, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null!clr-namespace:ReversiReversi  5          Reversi.HighscoresPage   5      W SPresentationCore, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35R NWindowsBase, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\ XPresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35D 9http://schemas.microsoft.com/winfx/2006/xaml/presentation    5      R NSystem.Xaml, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089:x,http://schemas.microsoft.com/winfx/2006/xaml     5      Jmc;http://schemas.openxmlformats.org/markup-compatibility/2006     5      @d2http://schemas.microsoft.com/expression/blend/2008     5      'localclr-namespace:Reversi     5        N� Title$  HighscoresPage��5	         BackgroundLinearGradient N� 
Background# �   6    � 	Resources 5
        EnterHeadingText �� TargetProperty �� 
TargetName O� KeyTime O� Value �� 	KeySpline  FadeInScores9  3System.Windows.Media.Animation.EasingDoubleKeyFrame��%�  &	       &	     �� 5   
   .��5      U� $U O(UIElement.RenderTransform).(TransformGroup.Children)[3].(TranslateTransform.X)�6,   $ HighscoresHeading��6�   .\�5      �� $ 0��6'   $
 -363Q�63   �� 5      $ 0:0:3��6'   $ 0Q�67   $ 0,1,0,1��6A   5      5      �� 5   
   .��5      U� $ (UIElement.Opacity)�6,   $ scrollViewer��6\   .\�5        $ 0��6'     5      $ 0:0:0.4��6'   $ 1Q�69   5      5      5       � Triggers 5      :� 5   
   	 :� RoutedEvent$	 FrameworkElement.Loaded��6   .[�5      �� #��[ 6   �� 5      #��[ 6   5      .��5      � 
 � RowDefinitions
 5   
   �� 5       .+�5"   
   �� -   5         ��Name$ HighscoresHeading��5"      $
��Left=�60   $��50,0,0,0q�6K   $	��Top=�6�    �� Orientation$ 
Horizontal=�6�   .+�5$      �� -   5         ��Name$
 Back��5$        WhiteIconButton#��[ 6#   $��60��6O    K� ZIndex$ 4��6Z   $��60��6k   $��0q�6w   $��5,0,0,0q�6�   $
��Left=�6�   $��Center=�6�   .��5%      �� $��Icons/backarrow_black.png��6   5&      �� 5(      .+�5)      �� $��72(�6   $
��Left=�6*   $��Segoe UI Light)�6E     PrimaryBrush#���  6a   $��
Highscores��6�   �� 5*      $��16(�6   $��2,0,0,0q�6*   �� 6<   $��Segoe UI Semibold)�6@   $�Everybody is a��6_     	GrayBrush#���  6u   �� 6�   $� ��6�   �� 6�   $��Segoe UI Semibold)�6�   $�	Winner :)��6�     
BlackBrush#���  6�   6  5+      �� 5-      -   5        $ Delete��5-      #��[ 6%   $��60��6Q   $ 4��6\   $��60��6m   $��0q�6y   $
��Left=�6�   $��50,0,0,0q�6�    � ToolTip$ Delete all game histories��6�   .��5.      �� $��Icons/delete_black.png��6   5/      z�50      V� 51      .~�52      �� �� 53      �� 54      T� 55      56      58      �� 5:   
   -   5         ��Name$ scrollViewer��5:      $��120,180,0,20q�6-   .��5;      �� -   5        $��
Highscores��5;      ��5<      �� 5=      .R�5>      �� $��16(�6$   $��4,5,0,0q�62     MedGrayBrush#���  6C   �� 5?      $��Segoe UI Semibold)�6   ���	#FFC37A7A�65   �� *	Player1+6N    �� 5@      $��Segoe UI)�6   ���	#FF8F8F8F�6,   �� *	WinType+6E    �� 5A      $��Segoe UI Semibold)�6   ���	#FFC37A7A�65   �� *	Player2+6N    �� 5B      $��Segoe UI)�6   ���	#FF8F8F8F�6,   �� *Score+6C   5C      5D      5F      5G      5I      5J      